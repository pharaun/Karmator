/* Create the simple state table */
CREATE TABLE simple_state (
    module_name VARCHAR NOT NULL,
    state_key VARCHAR NOT NULL,
    state BLOB NOT NULL,
    PRIMARY KEY (module_name, state_key)
);

/* Schema alter to add in the new metadata columns on votes */
ALTER TABLE votes RENAME TO votes_backup;
CREATE TABLE votes (
    id INTEGER PRIMARY KEY NOT NULL,
    voted_at DATETIME NOT NULL, /* UTC Timestamp */
    by_whom_name VARCHAR NOT NULL COLLATE nocase,
    for_what_name VARCHAR NOT NULL COLLATE nocase,
    amount INTEGER NOT NULL,
    /* Metadata */
    full_name VARCHAR NOT NULL,
    username VARCHAR DEFAULT NULL,
    hostmask VARCHAR DEFAULT NULL,
    channel VARCHAR DEFAULT NULL
);
INSERT INTO votes(id,voted_at,by_whom_name,for_what_name,amount,full_name) SELECT id,voted_at,by_whom_name,for_what_name,amount,by_whom_name FROM votes_backup;
DROP TABLE votes_backup;

/* Add some new triggers to ease maint of the database */
CREATE TRIGGER delete_increase_karma_count AFTER DELETE ON votes
  WHEN OLD.amount = 1 BEGIN
    UPDATE OR IGNORE karma_given_count SET up = up - 1 WHERE name = OLD.by_whom_name;
    UPDATE OR IGNORE karma_received_count SET up = up - 1 WHERE name = OLD.for_what_name;
  END;
CREATE TRIGGER delete_decrease_karma_count AFTER DELETE ON votes
  WHEN OLD.amount = -1 BEGIN
    UPDATE OR IGNORE karma_given_count SET down = down - 1 WHERE name = OLD.by_whom_name;
    UPDATE OR IGNORE karma_received_count SET down = down - 1 WHERE name = OLD.for_what_name;
  END;
CREATE TRIGGER delete_increase_sidevote_count AFTER DELETE ON votes
  WHEN OLD.amount = 0 BEGIN
    UPDATE OR IGNORE karma_given_count SET side = side - 1 WHERE name = OLD.by_whom_name;
    UPDATE OR IGNORE karma_received_count SET side = side - 1 WHERE name = OLD.for_what_name;
  END;

/* Reload old triggers */
CREATE TRIGGER increase_karma_count AFTER INSERT ON votes
  WHEN NEW.amount = 1 BEGIN
    INSERT OR IGNORE INTO karma_given_count VALUES (NULL, NEW.by_whom_name, 0, 0, 0);
    UPDATE karma_given_count SET up = up + 1 WHERE name = NEW.by_whom_name;
    INSERT OR IGNORE INTO karma_received_count VALUES (NULL, NEW.for_what_name, 0, 0, 0);
    UPDATE karma_received_count SET up = up + 1 WHERE name = NEW.for_what_name;
  END;
CREATE TRIGGER decrease_karma_count AFTER INSERT ON votes
  WHEN NEW.amount = -1 BEGIN
    INSERT OR IGNORE INTO karma_given_count VALUES (NULL, NEW.by_whom_name, 0, 0, 0);
    UPDATE karma_given_count SET down = down + 1 WHERE name = NEW.by_whom_name;
    INSERT OR IGNORE INTO karma_received_count VALUES (NULL, NEW.for_what_name, 0, 0, 0);
    UPDATE karma_received_count SET down = down + 1 WHERE name = NEW.for_what_name;
  END;
CREATE TRIGGER increase_sidevote_count AFTER INSERT ON votes
  WHEN NEW.amount = 0 BEGIN
    INSERT OR IGNORE INTO karma_given_count VALUES (NULL, NEW.by_whom_name, 0, 0, 0);
    UPDATE karma_given_count SET side = side + 1 WHERE name = NEW.by_whom_name;
    INSERT OR IGNORE INTO karma_received_count VALUES (NULL, NEW.for_what_name, 0, 0, 0);
    UPDATE karma_received_count SET side = side + 1 WHERE name = NEW.for_what_name;
  END;
