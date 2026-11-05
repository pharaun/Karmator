use std::time::Duration;

use anyhow::Result as AResult;
use anyhow::bail;

use tokio::time;
use tokio_util::sync::CancellationToken;

use karmator_tester::{Harness, ServerConfig, events};

async fn votes(harness: &Harness) -> AResult<i64> {
    let client = harness.pool().get().await?;
    Ok(client
        .query_one("SELECT count(*) FROM votes", &[])
        .await?
        .get(0))
}

async fn wait_for_votes(harness: &Harness, want: i64) -> AResult<()> {
    for _ in 0..100 {
        if votes(harness).await? == want {
            return Ok(());
        }
        time::sleep(Duration::from_millis(100)).await;
    }
    bail!("Expected {want} votes, got {}", votes(harness).await?);
}

#[tokio::test(flavor = "multi_thread")]
async fn karma_survives_reconnect_and_truncate() -> AResult<()> {
    let (watcher, harness) = start().await?;
    let handle = harness.handle();

    // Prove that the bot dedup and record this once
    handle.server.send_event(events::message("foo++"))?;
    wait_for_votes(&harness, 1).await?;

    // Prove that outage queue works
    handle.server.net_disconnect(Duration::from_secs(1))?;
    for _ in 0..10 {
        handle.server.send_event(events::message("bar++"))?;
    }
    wait_for_votes(&harness, 11).await?;

    handle.db.truncate().await?;
    assert_eq!(votes(&harness).await?, 0);

    handle.db.reset().await?;
    handle.server.send_event(events::message("baz++"))?;
    wait_for_votes(&harness, 1).await?;

    drop(handle);
    stop(watcher, harness).await
}

async fn start() -> AResult<(CancellationToken, Harness)> {
    let watcher = CancellationToken::new();
    let config = ServerConfig {
        refresh_interval: None,
        ..ServerConfig::default()
    };
    Ok((watcher.clone(), Harness::start(watcher, config).await?))
}

async fn stop(watcher: CancellationToken, harness: Harness) -> AResult<()> {
    watcher.cancel();
    time::timeout(Duration::from_secs(40), harness.run()).await?;
    Ok(())
}
