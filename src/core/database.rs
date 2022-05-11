use rusqlite as rs;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;

use chrono::DateTime;
use chrono::Local;

use tokio::sync::mpsc;
use tokio::sync::oneshot;
use tokio::time::Instant;

use futures::executor::block_on_stream;
use tokio_stream::wrappers::ReceiverStream;


pub type DbResult<T> = Result<T, String>;
pub type Query = Box<dyn FnOnce(&mut rs::Connection) -> DbResult<()> + Send + 'static>;
type Query2<T> = Box<dyn FnOnce(&mut rs::Connection) -> Result<T, Box<dyn std::error::Error>> + Send + 'static>;


pub async fn send_query<T>(
    sql_tx: &mut mpsc::Sender<Query>,
    query: Query2<T>,
) -> DbResult<T>
where
    T: Send + 'static
{
    let (tx, rx) = oneshot::channel();

    let query2 = move |conn: &mut rs::Connection| {
        let res = query(conn);

        match res {
            Ok(x)  => tx.send(x).map_err(|_| "Cant send ResQuery".to_string()),
            Err(x) => Err(x.to_string()),
        }
    };

    sql_tx.send(Box::new(query2)).await.map_err(|_| "Error sending query".to_string())?;
    rx.await.map_err(|_| "Error recieving".to_string())
}


pub fn process_queries(
    filename: &Path,
    sql_rx: ReceiverStream<Query>
) -> DbResult<()> {
    let mut block_sql_rx = block_on_stream(sql_rx);

    let mut conn = rs::Connection::open_with_flags(
        filename,
        rs::OpenFlags::SQLITE_OPEN_READ_WRITE
    ).expect(&format!("Connection error: {:?}", filename.to_str()));

    // Listen for inbound query
    while let Some(query) = block_sql_rx.next() {
        match query(&mut conn) {
            Ok(_) => (),
            Err(x) => eprintln!("{:?}", x),
        }
    }

    Ok(())
}


pub async fn backup(
    backup_path: String,
    mut sql_tx: mpsc::Sender<Query>,
) -> DbResult<()> {
    let local: DateTime<Local> = Local::now();
    let mut path = PathBuf::from(backup_path);
    path.push(
        format!(
            "db-backup-{}.sqlite",
            local.format("%F")
    ));

    if !path.exists() {
        // Run a backup now
        send_query(
            &mut sql_tx,
            Box::new(move |conn: &mut rs::Connection| {
                let mut dst = rs::Connection::open(
                    path.clone(),
                ).expect(&format!("Connection error: {:?}", path.to_str()));

                let start = Instant::now();
                println!("INFO [Sqlite Backup]: Starting backup");

                let backup = rs::backup::Backup::new(conn, &mut dst)?;
                backup.run_to_completion(100, Duration::ZERO, None)?;

                let done = Instant::now();
                println!("INFO [Sqlite Backup]: Done - {:?}", done.duration_since(start));
                Ok(())
            })
        ).await?;
    }

    Ok(())
}
