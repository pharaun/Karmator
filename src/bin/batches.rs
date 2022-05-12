#[macro_use]
extern crate clap;

use clap::{App, Arg, ArgMatches, SubCommand, AppSettings};
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

fn main() {
    let matches = App::new("Karmator maintance batch")
        .version(crate_version!())
        .author(crate_authors!("\n"))
        .about("Handles the maintance work for karmator")
        .setting(AppSettings::ColoredHelp)
        .subcommand(
            SubCommand::with_name("runs")
                .about("Detect runs of votes")
                .arg(
                    Arg::with_name("min")
                        .short("m")
                        .help("Min count of runs before outputting")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("delete")
                        .long("delete")
                        .help("Delete the runs detected"),
                )
                .arg(
                    Arg::with_name("FILE")
                        .help("Database file to operate on")
                        .required(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("prune")
                .about("Prune and pack up old backups")
                .arg(
                    Arg::with_name("delete")
                        .long("delete")
                        .help("Delete the old files"),
                )
                .arg(
                    Arg::with_name("BACKUPS")
                        .help("Backup directory to prune")
                        .required(true),
                ),
        )
        .get_matches();

    match matches.subcommand() {
        ("runs", Some(m))  => {
            let filename = m.value_of("FILE").unwrap();
            let min = value_t!(m, "min", u32).unwrap_or(20);
            let delete = m.is_present("delete");

            run(filename, min, delete)
        },
        ("prune", Some(m)) => {
            let directory = m.value_of("BACKUPS").unwrap();
            let delete = m.is_present("delete");

            prune(directory, delete)
        }
        _ => {
            println!("{}", matches.usage());
            Ok(())
        },
    };
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


fn prune(directory: &str, delete: bool) -> Result<(), Box<dyn Error>> {
    let now: DateTime<Local> = Local::now();
    let year = now.year();
    let month = now.month();

    // Fetch a set of all of the file in the current month+year
    let current_month_year = collect_glob(directory, &format!("/db-backup-{}-{:02}-??.sqlite.zst", year, month));

    // Fetch a set of all of the file that is in previous year + first of the month
    // If == 12, pack it up
    let previous_first_month = collect_glob(directory, &format!("/db-backup-{}-??-01.sqlite.zst", year - 1));

    // Fetch a set of all of the file that is in current year + first of the month
    // if == 12 pack it up
    let current_first_month = collect_glob(directory, &format!("/db-backup-{}-??-01.sqlite.zst", year));

    // Fetch a set of all of the files
    let all_files = collect_glob(directory, "/db-backup-????-??-??.sqlite.zst");

    println!("cmy: {:?}\n", current_month_year);
    println!("pfm: {:?}\n", previous_first_month);
    println!("cfm: {:?}\n", current_first_month);
    println!("All: {:?}\n", all_files);

    // Calculate what to keep and what to delete
    let delete = delete_set(&all_files, vec![&current_month_year, &previous_first_month, &current_first_month]);

    println!("DELETE: {:?}\n", delete);


    println!("cmy: {:?}", current_month_year.len());
    println!("pfm: {:?}", previous_first_month.len());
    println!("cfm: {:?}", current_first_month.len());
    println!("keeping: {:?}\n", current_month_year.len() + previous_first_month.len() + current_first_month.len());

    println!("All: {:?}", all_files.len());
    println!("DELETE: {:?}", delete.len());
    println!("keeping: {:?}", all_files.len() - delete.len());
    println!("Difference is cos of double counting the current_first_month + current_month_year's first day file");

    // Rules:
    //  - Keep current month of daily snapshot
    //  - Keep first of each month in a year
    //  - When there is 12 first month snapshot of a year, compact it into a tarfile



    Ok(())
}

fn collect_glob(directory: &str, glob_str: &str) -> HashSet<OsString> {
    glob(&(directory.to_string() + glob_str)).unwrap()
        .flatten()
        .flat_map(|e| e.file_name().map(|s| s.to_os_string()))
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
