/* Create the simple state table */
CREATE TABLE simple_state (
    module_name VARCHAR NOT NULL,
    state_key VARCHAR NOT NULL,
    state BLOB NOT NULL,
    PRIMARY KEY (module_name, state_key)
);

/* Schema alter to add in the new metadata columns on votes */
CREATE TABLE nick_metadata (
    id INTEGER PRIMARY KEY NOT NULL,
    cleaned_nick VARCHAR NOT NULL,
    full_name VARCHAR NOT NULL,
    username VARCHAR NOT NULL,
    hostmask VARCHAR NOT NULL,
    UNIQUE (cleaned_nick, full_name, username, hostmask)
);
CREATE TABLE chan_metadata (
    id INTEGER PRIMARY KEY NOT NULL,
    channel VARCHAR NOT NULL,
    UNIQUE (channel)
);

ALTER TABLE votes ADD COLUMN nick_id INTEGER DEFAULT NULL;
ALTER TABLE votes ADD COLUMN chan_id INTEGER DEFAULT NULL;

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
