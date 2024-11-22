/* Permission */
GRANT SELECT, INSERT ON TABLE nick_metadata TO $USER$;
GRANT SELECT, INSERT ON TABLE chan_metadata TO $USER$;
GRANT SELECT, INSERT ON TABLE reacji_message TO $USER$;

GRANT INSERT ON TABLE votes TO $USER$;
GRANT INSERT ON TABLE reacji_votes TO $USER$;

GRANT SELECT ON TABLE karma_given_count TO $USER$;
GRANT SELECT ON TABLE karma_received_count TO $USER$;
