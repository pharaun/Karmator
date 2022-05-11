use rusqlite as rs;
use std::path::{Path, PathBuf};
use std::time::Duration;
use std::fs::File;
use std::io::{SeekFrom, Seek, copy};

use zstd::stream::read::Encoder;

use chrono::{Local, DateTime};

use tokio::sync::{mpsc, oneshot};
use tokio::time::Instant;
use tokio::task;

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
            "db-backup-{}.sqlite.zst",
            local.format("%F")
    ));

    if !path.exists() {
        let (mut file, temp_path) = tempfile::NamedTempFile::new().unwrap().into_parts();

        // Run a backup now
        send_query(
            &mut sql_tx,
            Box::new(move |conn: &mut rs::Connection| {
                let mut dst = rs::Connection::open(
                    &temp_path
                ).expect(&format!("Connection error: {:?}", temp_path.to_str()));

                let start = Instant::now();
                println!("INFO [Sqlite Backup]: Starting backup");

                let backup = rs::backup::Backup::new(conn, &mut dst)?;
                backup.run_to_completion(100, Duration::ZERO, None)?;

                let done = Instant::now();
                println!("INFO [Sqlite Backup]: Backup Done - {:?}", done.duration_since(start));
                Ok(())
            })
        ).await?;

        // Compress the temporary file into ZST
        file.seek(SeekFrom::Start(0)).unwrap();

        task::spawn_blocking(move || {
            let start = Instant::now();
            println!("INFO [Sqlite Backup]: Starting compression");

            let mut out_file = File::create(path).unwrap();
            let mut comp = Encoder::new(
                &mut file,
                21
            ).unwrap();
            copy(&mut comp, &mut out_file).unwrap();

            let done = Instant::now();
            println!("INFO [Sqlite Backup]: Compression Done - {:?}", done.duration_since(start));
        }).await.unwrap();
    }

    Ok(())
}
