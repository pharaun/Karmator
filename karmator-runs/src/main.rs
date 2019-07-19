#[macro_use]
extern crate clap;

use clap::{Arg, App, SubCommand, ArgMatches};
use std::path::Path;
use rusqlite as rs;

use std::vec::Vec;


fn main() {
    let matches = App::new("Karmator db maintance batch")
        .version(crate_version!())
        .author(crate_authors!("\n"))
        .about("Handles the maintance work on the karmator db")
        .arg(Arg::with_name("FILE")
                .help("Database file to operate on")
                .required(true)
                .index(1))
        .subcommand(SubCommand::with_name("runs")
                .about("Detect runs of votes")
                .arg(Arg::with_name("min")
                    .short("m")
                    .help("Min count of runs before outputting")
                    .takes_value(true)))
        .get_matches();

    let filename = matches.value_of("FILE").unwrap_or("db.sqlite");
    match matches.subcommand() {
        ("runs", Some(m)) => run(m, filename),
        _ => Ok(()),
    };
}


#[derive(Debug, Clone)]
struct Vote {
    id: i32,
    by_whom_name: String,
    for_what_name: String,
    amount: i8,
}

#[derive(Debug)]
struct RunVal {
    oldest: Vote,
    newest: Vote,
    count: u32,
}

fn run(matches: &ArgMatches, filename: &str) -> Result<(), &'static str> {
    let min = value_t!(matches, "min", u32).unwrap_or(3);

    let conn = rs::Connection::open_with_flags(
        Path::new(filename),
        rs::OpenFlags::SQLITE_OPEN_READ_WRITE,
    ).unwrap();

    let mut stmt = conn.prepare("SELECT id, by_whom_name, for_what_name, amount FROM votes").unwrap();
    let vote_iter = stmt.query_map(rs::params![], |row| {
        Ok(Vote {
            id: row.get(0)?,
            by_whom_name: row.get(1)?,
            for_what_name: row.get(2)?,
            amount: row.get(3)?,
        })
    }).unwrap();

    // Time to compute the run
    let mut runs = Vec::new();

    let mut start_run_vote = None;
    let mut prev_vote = None;
    let mut count = 0;

    for rvote in vote_iter {
        let vote = rvote.unwrap();

        match (&start_run_vote, &prev_vote) {
            (None, None) => {
                start_run_vote = Some(vote.clone());
                prev_vote = Some(vote.clone());
                count = 1; // Run of 1
            },
            (Some(srv), Some(pv)) => {
                if (pv.by_whom_name == vote.by_whom_name) &&
                   (pv.for_what_name == vote.for_what_name) &&
                   (pv.amount == vote.amount) {
                    // Current vote + prev vote are the same, inc prev vote
                    prev_vote = Some(vote.clone());
                    count += 1;
                } else {
                    // Current vote != prev vote, record the
                    // Start_run_vote + prev_vote in the runs
                    // And reset the start_run_vote + prev_vote to current vote
                    // and reset count to 1
                    runs.push(RunVal {
                        oldest: (start_run_vote.unwrap()).clone(),
                        newest: (prev_vote.unwrap()).clone(),
                        count: count,
                    });

                    start_run_vote = Some(vote.clone());
                    prev_vote = Some(vote.clone());
                    count = 1; // Run of 1
                }
            },
            (_, _) => println!("Shouldn't happen"),
        };
    }

    // Record the last run
    match (start_run_vote, prev_vote) {
        (Some(srv), Some(pv)) => {
            runs.push(RunVal {
                oldest: srv,
                newest: pv,
                count: count,
            });
        },
        (_, _) => println!("Shouldn't happen 2"),
    };

    // Now we can scan for anything that > min and print them
    println!("start_id, end_id, by_whom_name, for_what_name, amount, count");
    for r in &runs {
        if r.count > min {
            println!("{}, {}, {}, {}, {}, {}", r.oldest.id, r.newest.id, r.oldest.by_whom_name, r.oldest.for_what_name, r.oldest.amount, r.count);
        }
    }

    Ok(())
}
