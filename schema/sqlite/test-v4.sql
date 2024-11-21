/* Test data for the v4 schema */
INSERT INTO chan_metadata (id, channel)
     VALUES (1, "CADEADBEE");

INSERT INTO nick_metadata (id, cleaned_nick, full_name, username, hostmask)
     VALUES (1, "pharaun", "Anja Berens", "UDEADBEEF", "SlackServer");

INSERT INTO votes (voted_at, by_whom_name, for_what_name, amount, nick_id, chan_id) VALUES
            (0, "A", "B", 1, 1, 1),  /* Upvote */
            (1, "C", "D", -1, 1, 1), /* Downvote */
            (2, "E", "F", 0, 1, 1);  /* Sidevote */

INSERT INTO reacji_message (id, ts, chan_id, nick_id, message) VALUES
            (1, 1, 1, 1, "G"),
            (2, 2, 1, 1, "H"),
            (3, 3, 1, 1, "I"),
            (4, 4, 1, 1, "J"),
            (5, 5, 1, 1, "K"),
            (6, 6, 1, 1, "L");

INSERT INTO reacji_votes (voted_at, by_whom_name, action, reacji_message_id, amount, nick_id) VALUES
            (0, "Z", 1, 1, 1, 1),  /* Add upvote */
            (0, "Y", 1, 2, -1, 1), /* Add downvote */
            (0, "X", 1, 3, 0, 1),  /* Add sidevote */
            (0, "W", 1, 4, 1, 1),  /* Add/Remove upvote */
            (0, "W", -1, 4, 1, 1),
            (0, "V", 1, 5, -1, 1), /* Add/Remove downvote */
            (0, "V", -1, 5, -1, 1),
            (0, "U", 1, 6, 0, 1),  /* Add/Remove sidevote */
            (0, "U", -1, 6, 0, 1);
