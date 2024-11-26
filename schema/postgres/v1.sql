/* Vote audit table */
CREATE TABLE votes (
	id BIGINT GENERATED BY DEFAULT AS IDENTITY,
	/* Seems like it will accept both old+new timestamp format: 2015-12-02 20:40:35.941993 UTC - 2015-12-02T23:51:04.351938 */
    voted_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    by_whom_name TEXT NOT NULL,
    for_what_name TEXT NOT NULL,
	/* 1 = add, -1 = remove, 0 = sidevote */
    amount SMALLINT NOT NULL,
    /* Metadata - Foreign key */
    nick_id INTEGER DEFAULT NULL,
    chan_id INTEGER DEFAULT NULL
);

/* Reaction vote audit table */
CREATE TABLE reacji_votes (
	id BIGINT GENERATED BY DEFAULT AS IDENTITY,
    voted_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    by_whom_name TEXT NOT NULL,
	/* Type of action (ie add a reacji or remove) 1 = add, -1 = remove */
    action SMALLINT NOT NULL,
	/* Foreign Key */
    reacji_message_id INTEGER NOT NULL,
	/* 1 = add, -1 = remove, 0 = sidevote */
    amount SMALLINT NOT NULL,
    /* Metadata - Foreign key */
    nick_id INTEGER NOT NULL
);

/* Audit metadata tables */
CREATE TABLE reacji_message (
	id BIGINT GENERATED BY DEFAULT AS IDENTITY,
	/* Slack timestamp, we do not touch it hence stored as TEXT */
    ts TEXT NOT NULL,
    chan_id INTEGER NOT NULL,
    nick_id INTEGER NOT NULL,
    message TEXT NOT NULL,
    UNIQUE (ts, chan_id)
);
CREATE TABLE nick_metadata (
	id BIGINT GENERATED BY DEFAULT AS IDENTITY,
    cleaned_nick TEXT NOT NULL,
    full_name TEXT NOT NULL,
    username TEXT NOT NULL,
    hostmask TEXT NOT NULL,
    UNIQUE (cleaned_nick, full_name, username, hostmask)
);
CREATE TABLE chan_metadata (
	id BIGINT GENERATED BY DEFAULT AS IDENTITY,
    channel TEXT NOT NULL,
    UNIQUE (channel)
);

/* Custom Collation to support existing karma_*_count tables */
CREATE COLLATION nocase (
	provider = icu,
	locale = 'und-u-ks-level2',
	deterministic = false
);

/* Summarized tables for querying */
CREATE TABLE karma_received_count (
	id BIGINT GENERATED BY DEFAULT AS IDENTITY,
    name TEXT UNIQUE NOT NULL COLLATE "nocase",
    up INTEGER NOT NULL DEFAULT 0,
    down INTEGER NOT NULL DEFAULT 0,
    side INTEGER NOT NULL DEFAULT 0
);
CREATE TABLE karma_given_count (
	id BIGINT GENERATED BY DEFAULT AS IDENTITY,
    name TEXT UNIQUE NOT NULL COLLATE "nocase",
    up INTEGER NOT NULL DEFAULT 0,
    down INTEGER NOT NULL DEFAULT 0,
    side INTEGER NOT NULL DEFAULT 0
);