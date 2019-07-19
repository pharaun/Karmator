/* Test data for the v3 schema */
INSERT INTO chan_metadata (id, channel) VALUES (1, "#test");

INSERT INTO nick_metadata (id, cleaned_nick, full_name, username, hostmask) VALUES (1, "asdf", "asdf", "asdf", "host");

INSERT INTO votes (voted_at, by_whom_name, for_what_name, amount, nick_id, chan_id) 
VALUES
    (0, "A", "B", 1, 1, 1),
    (0, "C", "D", -1, 1, 1),
    (0, "C", "D", -1, 1, 1),
    (0, "E", "F", 0, 1, 1),
    (0, "E", "F", 0, 1, 1),
    (0, "E", "F", 0, 1, 1);
