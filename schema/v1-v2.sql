/* Migrate the Votes table to have a integer primary id */
CREATE TEMP TABLE votes_backup (
    id INTEGER PRIMARY KEY,
    voted_at DATETIME NOT NULL, /* This is currently LocalTime, presumely PDT and PST */
    by_whom_name VARCHAR NOT NULL COLLATE nocase,
    for_what_name VARCHAR NOT NULL COLLATE nocase,
    amount INTEGER NOT NULL
);
INSERT INTO votes_backup(id,voted_at,by_whom_name,for_what_name,amount) SELECT rowid,voted_at,by_whom_name,for_what_name,amount FROM votes;
DROP TABLE votes;

CREATE TABLE votes (
    id INTEGER PRIMARY KEY,
    voted_at DATETIME NOT NULL,
    by_whom_name VARCHAR NOT NULL COLLATE nocase,
    for_what_name VARCHAR NOT NULL COLLATE nocase,
    amount INTEGER NOT NULL
);
INSERT INTO votes SELECT id,voted_at,by_whom_name,for_what_name,amount FROM votes_backup;
DROP TABLE votes_backup;

/* Migrate the karma received count table to have a integer primary id */
CREATE TEMP TABLE karma_received_count_backup (
    id INTEGER PRIMARY KEY,
    name VARCHAR UNIQUE NOT NULL COLLATE nocase,
    up INTEGER NOT NULL,
    down INTEGER NOT NULL,
    side INTEGER NOT NULL
);
INSERT INTO karma_received_count_backup(id,name,up,down,side) SELECT rowid,name,up,down,side FROM karma_received_count;
DROP TABLE karma_received_count;

CREATE TABLE karma_received_count (
    id INTEGER PRIMARY KEY,
    name VARCHAR UNIQUE NOT NULL COLLATE nocase,
    up INTEGER NOT NULL,
    down INTEGER NOT NULL,
    side INTEGER NOT NULL
);
INSERT INTO karma_received_count SELECT id,name,up,down,side FROM karma_received_count_backup;
DROP TABLE karma_received_count_backup;

/* Migrate the karma given count table to have a integer primary id */
CREATE TEMP TABLE karma_given_count_backup (
    id INTEGER PRIMARY KEY,
    name VARCHAR UNIQUE NOT NULL COLLATE nocase,
    up INTEGER NOT NULL,
    down INTEGER NOT NULL,
    side INTEGER NOT NULL
);
INSERT INTO karma_given_count_backup(id,name,up,down,side) SELECT rowid,name,up,down,side FROM karma_given_count;
DROP TABLE karma_given_count;

CREATE TABLE karma_given_count (
    id INTEGER PRIMARY KEY,
    name VARCHAR UNIQUE NOT NULL COLLATE nocase,
    up INTEGER NOT NULL,
    down INTEGER NOT NULL,
    side INTEGER NOT NULL
);
INSERT INTO karma_given_count SELECT id,name,up,down,side FROM karma_given_count_backup;
DROP TABLE karma_given_count_backup;

/* Cleanup the empty sidevotes table */
DROP TABLE sidevotes;


/* For now migrate the old karma and oldkrc tables as well */
CREATE TEMP TABLE oldkarma_backup (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    normalized TEXT UNIQUE NOT NULL,
    added INT NOT NULL,
    subtracted INT NOT NULL
);
INSERT INTO oldkarma_backup(id,name,normalized,added,subtracted) SELECT id,name,normalized,added,subtracted FROM oldkarma;
DROP TABLE oldkarma;

CREATE TABLE oldkarma (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    normalized TEXT UNIQUE NOT NULL,
    added INT NOT NULL,
    subtracted INT NOT NULL
);
INSERT INTO oldkarma SELECT id,name,normalized,added,subtracted FROM oldkarma_backup;
DROP TABLE oldkarma_backup;

/* For now migrate the old karma and oldkrc tables as well */
CREATE TEMP TABLE oldkrc_backup (
    id INTEGER PRIMARY KEY,
    name VARCHAR UNIQUE NOT NULL COLLATE nocase,
    up INTEGER NOT NULL,
    down INTEGER NOT NULL,
    side INTEGER NOT NULL
);
INSERT INTO oldkrc_backup(id,name,up,down,side) SELECT rowid,name,up,down,side FROM oldkrc;
DROP TABLE oldkrc;

CREATE TABLE oldkrc (
    id INTEGER PRIMARY KEY,
    name VARCHAR UNIQUE NOT NULL COLLATE nocase,
    up INTEGER NOT NULL,
    down INTEGER NOT NULL,
    side INTEGER NOT NULL
);
INSERT INTO oldkrc SELECT id,name,up,down,side FROM oldkrc_backup;
DROP TABLE oldkrc_backup;


/* Reinstall the triggers */
CREATE TRIGGER increase_karma_count AFTER INSERT ON votes
  WHEN NEW.amount = 1 BEGIN
    INSERT OR IGNORE INTO karma_given_count VALUES (NEW.by_whom_name, 0, 0, 0);
    UPDATE karma_given_count SET up = up + 1 WHERE name = NEW.by_whom_name;
    INSERT OR IGNORE INTO karma_received_count VALUES (NEW.for_what_name, 0, 0, 0);
    UPDATE karma_received_count SET up = up + 1 WHERE name = NEW.for_what_name;
  END;
CREATE TRIGGER decrease_karma_count AFTER INSERT ON votes
  WHEN NEW.amount = -1 BEGIN
    INSERT OR IGNORE INTO karma_given_count VALUES (NEW.by_whom_name, 0, 0, 0);
    UPDATE karma_given_count SET down = down + 1 WHERE name = NEW.by_whom_name;
    INSERT OR IGNORE INTO karma_received_count VALUES (NEW.for_what_name, 0, 0, 0);
    UPDATE karma_received_count SET down = down + 1 WHERE name = NEW.for_what_name;
  END;
CREATE TRIGGER increase_sidevote_count AFTER INSERT ON votes
  WHEN NEW.amount = 0 BEGIN
    INSERT OR IGNORE INTO karma_given_count VALUES (NEW.by_whom_name, 0, 0, 0);
    UPDATE karma_given_count SET side = side + 1 WHERE name = NEW.by_whom_name;
    INSERT OR IGNORE INTO karma_received_count VALUES (NEW.for_what_name, 0, 0, 0);
    UPDATE karma_received_count SET side = side + 1 WHERE name = NEW.for_what_name;
  END;
