use tokio_util::sync::CancellationToken;

#[cfg(unix)]
pub async fn wait_for_signal(watcher: CancellationToken) {
    use tokio::signal::unix::SignalKind;
    use tokio::signal::unix::signal;

    let mut sig_int = signal(SignalKind::interrupt()).expect("Unix Signal");
    let mut sig_term = signal(SignalKind::terminate()).expect("Unix Signal");

    // Wait for a cancellation request or a signal
    tokio::select! {
        () = watcher.cancelled() => (),
        _ = sig_int.recv() => (),
        _ = sig_term.recv() => (),
    };

    // In case that the reason we exited the select! was cuz of a signal,
    // go ahead and trigger the shutdown token
    watcher.cancel();
}

#[cfg(windows)]
pub async fn wait_for_signal(watcher: CancellationToken) {
    use tokio::signal::windows;

    let mut ctrl_c = windows::ctrl_c().expect("Windows Signal");
    let mut ctrl_break = windows::ctrl_break().expect("Windows Signal");
    let mut ctrl_close = windows::ctrl_close().expect("Windows Signal");
    let mut ctrl_shutdown = windows::ctrl_shutdown().expect("Windows Signal");

    // Wait for a cancellation request or a signal
    tokio::select! {
        _ = watcher.cancelled() => (),
        _ = ctrl_c.recv() => (),
        _ = ctrl_break.recv() => (),
        _ = ctrl_close.recv() => (),
        _ = ctrl_shutdown.recv() => (),
    };

    // In case that the reason we exited the select! was cuz of a signal,
    // go ahead and trigger the shutdown token
    watcher.cancel();
}
