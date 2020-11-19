use rusqlite as rs;

pub fn setup_table(
    conn: &rs::Connection
) -> rs::Result<()> {
    println!("Sql Worker - Setup - votes");
    conn.execute(
        "CREATE TABLE votes (
            id INTEGER PRIMARY KEY NOT NULL,
            voted_at DATETIME NOT NULL, /* UTC Timestamp */
            by_whom_name VARCHAR NOT NULL COLLATE nocase,
            for_what_name VARCHAR NOT NULL COLLATE nocase,
            amount INTEGER NOT NULL,
            /* Metadata */
            nick_id INTEGER DEFAULT NULL,
            chan_id INTEGER DEFAULT NULL
        )",
        rs::params![],
    )?;

    println!("Sql Worker - Setup - nick_metadata");
    conn.execute(
        "CREATE TABLE nick_metadata (
            id INTEGER PRIMARY KEY NOT NULL,
            cleaned_nick VARCHAR NOT NULL,
            full_name VARCHAR NOT NULL,
            username VARCHAR NOT NULL,
            hostmask VARCHAR NOT NULL,
            UNIQUE (cleaned_nick, full_name, username, hostmask)
        )",
        rs::params![],
    )?;

    println!("Sql Worker - Setup - chan_metadata");
    conn.execute(
        "CREATE TABLE chan_metadata (
            id INTEGER PRIMARY KEY NOT NULL,
            channel VARCHAR NOT NULL,
            UNIQUE (channel)
        )",
        rs::params![],
    )?;

    println!("Sql Worker - Setup - karma_received_count");
    conn.execute(
        "CREATE TABLE karma_received_count (
            id INTEGER PRIMARY KEY NOT NULL,
            name VARCHAR UNIQUE NOT NULL COLLATE nocase,
            up INTEGER NOT NULL,
            down INTEGER NOT NULL,
            side INTEGER NOT NULL
        )",
        rs::params![],
    )?;

    println!("Sql Worker - Setup - karma_given_count");
    conn.execute(
        "CREATE TABLE karma_given_count (
            id INTEGER PRIMARY KEY NOT NULL,
            name VARCHAR UNIQUE NOT NULL COLLATE nocase,
            up INTEGER NOT NULL,
            down INTEGER NOT NULL,
            side INTEGER NOT NULL
        )",
        rs::params![],
    )?;

    println!("Sql Worker - Setup - Triggers");
    conn.execute(
        "CREATE TRIGGER increase_karma_count AFTER INSERT ON votes
          WHEN NEW.amount = 1 BEGIN
            INSERT OR IGNORE INTO karma_given_count VALUES (NULL, NEW.by_whom_name, 0, 0, 0);
            UPDATE karma_given_count SET up = up + 1 WHERE name = NEW.by_whom_name;
            INSERT OR IGNORE INTO karma_received_count VALUES (NULL, NEW.for_what_name, 0, 0, 0);
            UPDATE karma_received_count SET up = up + 1 WHERE name = NEW.for_what_name;
          END",
        rs::params![],
    )?;
    conn.execute(
        "CREATE TRIGGER delete_increase_karma_count AFTER DELETE ON votes
          WHEN OLD.amount = 1 BEGIN
            UPDATE OR IGNORE karma_given_count SET up = up - 1 WHERE name = OLD.by_whom_name;
            UPDATE OR IGNORE karma_received_count SET up = up - 1 WHERE name = OLD.for_what_name;
          END",
        rs::params![],
    )?;
    conn.execute(
        "CREATE TRIGGER decrease_karma_count AFTER INSERT ON votes
          WHEN NEW.amount = -1 BEGIN
            INSERT OR IGNORE INTO karma_given_count VALUES (NULL, NEW.by_whom_name, 0, 0, 0);
            UPDATE karma_given_count SET down = down + 1 WHERE name = NEW.by_whom_name;
            INSERT OR IGNORE INTO karma_received_count VALUES (NULL, NEW.for_what_name, 0, 0, 0);
            UPDATE karma_received_count SET down = down + 1 WHERE name = NEW.for_what_name;
          END",
        rs::params![],
    )?;
    conn.execute(
        "CREATE TRIGGER delete_decrease_karma_count AFTER DELETE ON votes
          WHEN OLD.amount = -1 BEGIN
            UPDATE OR IGNORE karma_given_count SET down = down - 1 WHERE name = OLD.by_whom_name;
            UPDATE OR IGNORE karma_received_count SET down = down - 1 WHERE name = OLD.for_what_name;
          END",
        rs::params![],
    )?;
    conn.execute(
        "CREATE TRIGGER increase_sidevote_count AFTER INSERT ON votes
          WHEN NEW.amount = 0 BEGIN
            INSERT OR IGNORE INTO karma_given_count VALUES (NULL, NEW.by_whom_name, 0, 0, 0);
            UPDATE karma_given_count SET side = side + 1 WHERE name = NEW.by_whom_name;
            INSERT OR IGNORE INTO karma_received_count VALUES (NULL, NEW.for_what_name, 0, 0, 0);
            UPDATE karma_received_count SET side = side + 1 WHERE name = NEW.for_what_name;
          END",
        rs::params![],
    )?;
    conn.execute(
        "CREATE TRIGGER delete_increase_sidevote_count AFTER DELETE ON votes
          WHEN OLD.amount = 0 BEGIN
            UPDATE OR IGNORE karma_given_count SET side = side - 1 WHERE name = OLD.by_whom_name;
            UPDATE OR IGNORE karma_received_count SET side = side - 1 WHERE name = OLD.for_what_name;
          END",
        rs::params![],
    )?;

    Ok(())
}

pub fn setup_data(
    conn: &rs::Connection
) -> rs::Result<()> {
    let ins = "INSERT INTO votes (voted_at, by_whom_name, for_what_name, amount) VALUES (?1, ?2, ?3, ?4)";

    println!("Sql Worker - Setup - Inserting Up-Votes");
    conn.execute(ins, rs::params!["2020", "a", "b", 1])?;
    conn.execute(ins, rs::params!["2020", "a", "b", 1])?;
    conn.execute(ins, rs::params!["2020", "a", "b", 1])?;
    conn.execute(ins, rs::params!["2020", "a", "b", 1])?;

    println!("Sql Worker - Setup - Inserting Down-Votes");
    conn.execute(ins, rs::params!["2020", "b", "c", -1])?;
    conn.execute(ins, rs::params!["2020", "b", "c", -1])?;
    conn.execute(ins, rs::params!["2020", "b", "c", -1])?;
    conn.execute(ins, rs::params!["2020", "b", "c", -1])?;

    println!("Sql Worker - Setup - Inserting Side-Votes");
    conn.execute(ins, rs::params!["2020", "c", "a", 0])?;
    conn.execute(ins, rs::params!["2020", "c", "a", 0])?;
    conn.execute(ins, rs::params!["2020", "c", "a", 0])?;
    conn.execute(ins, rs::params!["2020", "c", "a", 0])?;

    Ok(())
}
