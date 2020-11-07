CREATE TABLE votes (
    voted_at DATETIME,
    by_whom_name VARCHAR COLLATE nocase,
    for_what_name VARCHAR NOT NULL COLLATE nocase,
    amount INTEGER NOT NULL
);

CREATE TABLE karma_received_count (
    name VARCHAR PRIMARY KEY COLLATE nocase,
    up INTEGER NOT NULL,
    down INTEGER NOT NULL,
    side INTEGER NOT NULL
);

CREATE TABLE karma_given_count (
    name VARCHAR PRIMARY KEY COLLATE nocase,
    up INTEGER NOT NULL,
    down INTEGER NOT NULL,
    side INTEGER NOT NULL
);

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
