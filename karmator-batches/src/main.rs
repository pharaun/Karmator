#[macro_use]
extern crate clap;

use clap::{App, Arg, ArgMatches, SubCommand};
use rusqlite as rs;
use std::path::Path;

use std::error::Error;
use std::vec::Vec;

fn main() {
    let matches = App::new("Karmator db maintance batch")
        .version(crate_version!())
        .author(crate_authors!("\n"))
        .about("Handles the maintance work on the karmator db")
        .arg(
            Arg::with_name("FILE")
                .help("Database file to operate on")
                .required(true)
                .index(1),
        )
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
                ),
        )
        .get_matches();

    let filename = matches.value_of("FILE").unwrap_or("db.sqlite");
    match matches.subcommand() {
        ("runs", Some(m)) => run(m, filename),
        _ => {
            println!("{}", matches.usage());
            Ok(())
        }
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

fn run(matches: &ArgMatches, filename: &str) -> Result<(), Box<dyn Error>> {
    let min = value_t!(matches, "min", u32).unwrap_or(10);
    let delete = matches.is_present("delete");

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
