/* Drop the simple state table */
DROP TABLE IF EXISTS simple_state;

/* Add the reacji tables */
CREATE TABLE reacji_votes (
    id INTEGER PRIMARY KEY NOT NULL,
    voted_at DATETIME NOT NULL, /* UTC Timestamp */
    by_whom_name VARCHAR NOT NULL,
    action INTEGER NOT NULL, /* 1 = add, -1 = remove */
    reacji_message_id INTEGER NOT NULL,
    amount INTEGER NOT NULL,
    nick_id INTEGER NOT NULL
);
CREATE TABLE reacji_message (
    id INTEGER PRIMARY KEY NOT NULL,
    ts VARCHAR NOT NULL,
    chan_id INTEGER NOT NULL,
    nick_id INTEGER NOT NULL,
    message VARCHAR NOT NULL,
    UNIQUE (ts, chan_id)
);

/* Add new reacji triggers */
CREATE TRIGGER increase_reacji_karma_count AFTER INSERT ON reacji_votes
  WHEN NEW.amount = 1 BEGIN
    INSERT OR IGNORE INTO karma_given_count VALUES (NULL, NEW.by_whom_name, 0, 0, 0);
    UPDATE karma_given_count SET up = up + NEW.action WHERE name = NEW.by_whom_name;
    INSERT OR IGNORE INTO karma_received_count VALUES (NULL, (SELECT message FROM reacji_message WHERE ID = NEW.reacji_message_id), 0, 0, 0);
    UPDATE karma_received_count SET up = up + NEW.action  WHERE name = (SELECT message FROM reacji_message WHERE ID = NEW.reacji_message_id);
  END;

CREATE TRIGGER delete_increase_reacji_karma_count_add AFTER DELETE ON reacji_votes
  WHEN OLD.amount = 1 BEGIN
    UPDATE OR IGNORE karma_given_count SET up = up - OLD.action  WHERE name = OLD.by_whom_name;
    UPDATE OR IGNORE karma_received_count SET up = up - OLD.action WHERE name = (SELECT message FROM reacji_message where ID = OLD.reacji_message_id);
  END;

CREATE TRIGGER decrease_reacji_karma_count AFTER INSERT ON reacji_votes
  WHEN NEW.amount = -1 BEGIN
    INSERT OR IGNORE INTO karma_given_count VALUES (NULL, NEW.by_whom_name, 0, 0, 0);
    UPDATE karma_given_count SET down = down + NEW.action WHERE name = NEW.by_whom_name;
    INSERT OR IGNORE INTO karma_received_count VALUES (NULL, (SELECT message FROM reacji_message WHERE ID = NEW.reacji_message_id), 0, 0, 0);
    UPDATE karma_received_count SET down = down + NEW.action  WHERE name = (SELECT message FROM reacji_message WHERE ID = NEW.reacji_message_id);
  END;

CREATE TRIGGER delete_decrease_reacji_karma_count_add AFTER DELETE ON reacji_votes
  WHEN OLD.amount = -1 BEGIN
    UPDATE OR IGNORE karma_given_count SET down = down - OLD.action  WHERE name = OLD.by_whom_name;
    UPDATE OR IGNORE karma_received_count SET down = down - OLD.action WHERE name = (SELECT message FROM reacji_message where ID = OLD.reacji_message_id);
  END;

CREATE TRIGGER increase_sidevote_reacji_karma_count AFTER INSERT ON reacji_votes
  WHEN NEW.amount = 0 BEGIN
    INSERT OR IGNORE INTO karma_given_count VALUES (NULL, NEW.by_whom_name, 0, 0, 0);
    UPDATE karma_given_count SET side = side + NEW.action WHERE name = NEW.by_whom_name;
    INSERT OR IGNORE INTO karma_received_count VALUES (NULL, (SELECT message FROM reacji_message WHERE ID = NEW.reacji_message_id), 0, 0, 0);
    UPDATE karma_received_count SET side = side + NEW.action  WHERE name = (SELECT message FROM reacji_message WHERE ID = NEW.reacji_message_id);
  END;

CREATE TRIGGER delete_increase_sidevote_reacji_karma_count_add AFTER DELETE ON reacji_votes
  WHEN OLD.amount = 0 BEGIN
    UPDATE OR IGNORE karma_given_count SET side = side - OLD.action  WHERE name = OLD.by_whom_name;
    UPDATE OR IGNORE karma_received_count SET side = side - OLD.action WHERE name = (SELECT message FROM reacji_message where ID = OLD.reacji_message_id);
  END;
