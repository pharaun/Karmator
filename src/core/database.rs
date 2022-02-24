use rusqlite as rs;
use std::path::Path;

use tokio::sync::mpsc;
use tokio::sync::oneshot;

use futures::executor::block_on_stream;
use tokio_stream::wrappers::ReceiverStream;


pub type Query = Box<dyn FnOnce(&mut rs::Connection) -> Result<(), String> + Send + 'static>;
type Query2<T> = Box<dyn FnOnce(&mut rs::Connection) -> Result<T, String> + Send + 'static>;

pub async fn send_query<T>(
    sql_tx: &mut mpsc::Sender<Query>,
    query: Query2<T>,
) -> Result<T, &'static str>
where
    T: Send + 'static
{
    let (tx, rx) = oneshot::channel();

    let query2 = move |conn: &mut rs::Connection| {
        let res = query(conn);

        match res {
            Ok(x)  => tx.send(x).map_err(|_| "Cant send ResQuery".to_string()),
            Err(x) => Err(x),
        }
    };

    sql_tx.send(Box::new(query2)).await.map_err(|_| "Error sending query")?;
    rx.await.map_err(|_| "Error recieving")
}


// TODO: additional check for shutting down
// if panic, trigger shutdown, if regular query error, log
pub fn process_queries(
    filename: &Path,
    sql_rx: ReceiverStream<Query>
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
