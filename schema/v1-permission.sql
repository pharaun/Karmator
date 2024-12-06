\set user ASDF

/* Table */
GRANT SELECT, INSERT ON TABLE nick_metadata TO :user;
GRANT SELECT, INSERT ON TABLE chan_metadata TO :user;
GRANT SELECT, INSERT ON TABLE reacji_message TO :user;

GRANT SELECT, INSERT ON TABLE votes TO :user;
GRANT SELECT, INSERT ON TABLE reacji_votes TO :user;

GRANT SELECT, INSERT, UPDATE ON TABLE karma_given_count TO :user;
GRANT SELECT, INSERT, UPDATE ON TABLE karma_received_count TO :user;

/* Triggers */
GRANT EXECUTE ON FUNCTION delete_karma_count() TO :user;
GRANT EXECUTE ON FUNCTION delete_reacji_karma_count() TO :user;
GRANT EXECUTE ON FUNCTION insert_karma_count() TO :user;
GRANT EXECUTE ON FUNCTION insert_reacji_karma_count() TO :user;

