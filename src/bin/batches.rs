#[macro_use]
extern crate clap;

use clap::{Command, Arg};
use rusqlite as rs;
use std::path::Path;

use std::error::Error;
use std::vec::Vec;
use std::ffi::OsString;
use std::collections::HashSet;

use chrono::Local;
use chrono::DateTime;
use chrono::Datelike;
use glob::glob;

use count_write::CountWrite;
use zstd::stream::copy_decode;
use zstd::stream::write::Encoder;
use zstd::stream::read::Decoder;
use tar::Builder;
use tar::Header;
use std::io::{SeekFrom, Seek};
use std::fs::File;
use std::fs::OpenOptions;
use std::ffi::OsStr;

fn main() {
    let matches = Command::new("Karmator maintance batch")
        .version(crate_version!())
        .author(crate_authors!("\n"))
        .about("Handles the maintance work for karmator")
        .subcommand(
            Command::new("runs")
                .about("Detect runs of votes")
                .arg(
                    Arg::new("min")
                        .short('m')
                        .help("Min count of runs before outputting")
                        .default_value("20"),
                )
                .arg(
                    Arg::new("delete")
                        .long("delete")
                        .help("Delete the runs detected"),
                )
                .arg(
                    Arg::new("FILE")
                        .help("Database file to operate on")
                        .required(true),
                ),
        )
        .subcommand(
            Command::new("prune")
                .about("Prune and pack up old backups")
                .arg(
                    Arg::new("delete")
                        .long("delete")
                        .help("Delete the old files"),
                )
                .arg(
                    Arg::new("skip")
                        .long("skip")
                        .help("Skip compacting old files"),
                )
                .arg(
                    Arg::new("BACKUPS")
                        .help("Backup directory to prune")
                        .required(true),
                ),
        )
        .get_matches();

    match matches.subcommand() {
        Some(("runs", m))  => {
            let filename = m.get_one::<String>("FILE").unwrap();
            let min = m.get_one::<u32>("min").unwrap();
            let delete = m.contains_id("delete");

            run(filename, *min, delete)
        },
        Some(("prune", m)) => {
            let directory = m.get_one::<String>("BACKUPS").unwrap();
            let delete = m.contains_id("delete");
            let skip = m.contains_id("skip");

            prune(directory, delete, skip)
        }
        _ => {
            println!("meh do --help yourself");
            Ok(())
        },
    }.unwrap();
}

#[derive(Debug, Clone)]
struct Vote {
    id: i32,
    by_whom_name: String,
    for_what_name: String,
    amount: i8,
}
impl PartialEq for Vote {
    fn eq(&self, other: &Self) -> bool {
        (self.by_whom_name == other.by_whom_name)
            && (self.for_what_name == other.for_what_name)
            && (self.amount == other.amount)
    }
}

#[derive(Debug)]
struct RunVal {
    oldest_id: i32,
    newest_id: i32,
    by_whom_name: String,
    for_what_name: String,
    amount: i8,
    count: u32,
}

fn get_run_val(srv: &Vote, pv: &Vote, count: u32) -> RunVal {
    RunVal {
        oldest_id: srv.id,
        newest_id: pv.id,
        by_whom_name: srv.by_whom_name.clone(),
        for_what_name: srv.for_what_name.clone(),
        amount: srv.amount,
        count: count,
    }
}

fn str_amount(amount: i8) -> &'static str {
    match amount {
        -1 => "Down",
        0 => "Side",
        1 => "Up",
        _ => panic!("invalid amount"),
    }
}

fn run(filename: &str, min: u32, delete: bool) -> Result<(), Box<dyn Error>> {
    let conn =
        rs::Connection::open_with_flags(Path::new(filename), rs::OpenFlags::SQLITE_OPEN_READ_WRITE)
            .expect(&format!("Connection error: {}", filename));

    let mut stmt = conn.prepare("SELECT id, by_whom_name, for_what_name, amount FROM votes")?;
    let vote_iter = stmt.query_map(rs::params![], |row| {
        Ok(Vote {
            id: row.get(0)?,
            by_whom_name: row.get(1)?,
            for_what_name: row.get(2)?,
            amount: row.get(3)?,
        })
    })?;

    // Time to compute the run
    let mut runs = Vec::new();

    let mut start_run_vote = None;
    let mut prev_vote = None;
    let mut count = 0;

    for rvote in vote_iter {
        let vote = rvote?;

        match (&start_run_vote, &prev_vote) {
            (None, None) => {
                start_run_vote = Some(vote.clone());
                prev_vote = Some(vote);
                count = 1; // Run of 1
            }
            (Some(srv), Some(pv)) => {
                if pv == &vote {
                    // Current vote + prev vote are the same, inc prev vote
                    prev_vote = Some(vote);
                    count += 1;
                } else {
                    // Current vote != prev vote, record the run, and reset
                    runs.push(get_run_val(srv, pv, count));

                    start_run_vote = Some(vote.clone());
                    prev_vote = Some(vote);
                    count = 1; // Run of 1
                }
            }
            (_, _) => panic!("Shouldn't happen"),
        };
    }

    // Record the last run
    runs.push(get_run_val(
        &start_run_vote.unwrap(),
        &prev_vote.unwrap(),
        count,
    ));

    if delete {
        // Scan and delete the offenders
        let mut stmt = conn.prepare("DELETE FROM votes WHERE id >= ? and id <= ?")?;
        for r in &runs {
            if r.count > min {
                let deleted = stmt.execute(rs::params![r.oldest_id, r.newest_id])?;

                if (r.count as usize) != deleted {
                    panic!("Expected: {} to be deleted, got {}", r.count, deleted);
                }
            }
        }
    } else {
        // Now we can scan for anything that > min and print them
        println!(
            "{: >8}, {: >8}, {: >14.14}, {: >14.14}, {: >6}, {: >5}",
            "start_id", "end_id", "by_whom_name", "for_what_name", "amount", "count"
        );
        for r in &runs {
            if r.count > min {
                println!(
                    "{: >8}, {: >8}, {: >14.14}, {: >14.14}, {: >6}, {: >5}",
                    r.oldest_id,
                    r.newest_id,
                    r.by_whom_name,
                    r.for_what_name,
                    str_amount(r.amount),
                    r.count
                );
            }
        }
    }

    Ok(())
}


fn prune(directory: &str, delete: bool, skip: bool) -> Result<(), Box<dyn Error>> {
    let now: DateTime<Local> = Local::now();
    let year = now.year();
    let month = now.month();

    // Fetch a set of all of the files
    let all_files = collect_glob(directory, "/db-backup-????-??-??.sqlite.zst");

    // Fetch a set of all of the file in the current month+year
    let current_month_year = collect_glob(directory, &format!("/db-backup-{}-{:02}-??.sqlite.zst", year, month));

    // Fetch a set of all of the file that is in previous year + first of the month
    let previous_first_month = collect_glob(directory, &format!("/db-backup-{}-??-01.sqlite.zst", year - 1));

    // Fetch a set of all of the file that is in current year + first of the month
    let current_first_month = collect_glob(directory, &format!("/db-backup-{}-??-01.sqlite.zst", year));

    // Calculate the initial set of files to prune
    let mut delete_files = delete_set(&all_files, vec![&current_month_year, &previous_first_month, &current_first_month]);

    // Compact pfm + cfm into their years
    if skip {
        println!("Compacting: Skipped");
    } else {
        if previous_first_month.len() == 12 {
            let tarfile = format!("{}/db-backup-{}.tar.zst", directory, year-1);
            print_compact(&previous_first_month, &tarfile)?;
            delete_files.extend(previous_first_month.iter().map(|e| e.clone()));
        }

        if current_first_month.len() == 12 {
            let tarfile = format!("{}/db-backup-{}.tar.zst", directory, year);
            print_compact(&current_first_month, &tarfile)?;
            delete_files.extend(current_first_month.iter().map(|e| e.clone()));
        }
    }

    // List the files we are going to delete
    print_delete(&delete_files, delete)?;
    Ok(())
}

fn collect_glob(directory: &str, glob_str: &str) -> HashSet<OsString> {
    glob(&(directory.to_string() + glob_str)).unwrap()
        .flatten()
        .map(|e| e.into_os_string())
        .collect::<HashSet<OsString>>()
}

fn delete_set(all: &HashSet<OsString>, keep: Vec<&HashSet<OsString>>) -> HashSet<OsString> {
    let mut delete = all.clone();

    for hs in keep {
        let out = delete.difference(&hs).map(|e| e.clone()).collect();
        delete = out;
    }

    delete
}

fn compact(compact: &HashSet<OsString>, filename: &str) -> Result<(), Box<dyn Error>> {
    let tarfile = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(filename);

    let mut tar = Builder::new(Encoder::new(tarfile?, 21)?.auto_finish());

    for f in compact.iter() {
        let mut file = File::open(f)?;
        let filename = Path::new(f).file_name().unwrap();
        let filesize = {
            let mut count = CountWrite::from(std::io::sink());
            copy_decode(&file, &mut count)?;
            count.count()
        };

        let mut header = Header::new_gnu();
        header.set_path(filename)?;
        header.set_size(filesize);
        header.set_cksum();

        file.seek(SeekFrom::Start(0))?;
        tar.append(
            &header,
            Decoder::new(std::fs::File::open(f)?)?
        )?;
    }

    tar.finish()?;

    Ok(())
}

fn print_compact(to_compact: &HashSet<OsString>, tarfile: &str) -> Result<(), Box<dyn Error>> {
    println!("Compacting: {}", tarfile);

    let mut print = to_compact.iter()
        .map(|e| Path::new(e).file_name().unwrap())
        .collect::<Vec<&OsStr>>();
    print.sort();

    for i in print.iter() {
        println!("\t{:?}", i);
    }

    compact(&to_compact, &tarfile)?;
    Ok(())
}

fn print_delete(to_delete: &HashSet<OsString>, delete: bool) -> Result<(), Box<dyn Error>> {
    println!("Deleting:");

    let mut print = to_delete.iter().collect::<Vec<&OsString>>();
    print.sort();

    for i in print.iter() {
        let path = Path::new(i);
        println!("\t{:?}", path.file_name().unwrap());

        if delete {
            std::fs::remove_file(path)?;
        }
    }
    Ok(())
}
