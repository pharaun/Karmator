use tokio::sync::mpsc;

// SQlite worker thread
use futures::executor::block_on_stream;

use rusqlite as rs;
use std::path::Path;


pub type Query = Box<dyn FnOnce(&mut rs::Connection) -> Result<(), String> + Send + 'static>;


// TODO: additional check for shutting down
// if panic, trigger shutdown, if regular query error, log
pub fn process_queries(
    filename: &Path,
    sql_rx: mpsc::Receiver<Query>
) -> Result<(), Box<dyn std::error::Error>> {
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
