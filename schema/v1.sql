/* Vote audit table */
CREATE TABLE votes (
    id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    /* Seems like it will accept both old+new timestamp format: 2015-12-02 20:40:35.941993 UTC - 2015-12-02T23:51:04.351938 */
    voted_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    by_whom_name TEXT NOT NULL,
    for_what_name TEXT NOT NULL,
    /* 1 = add, -1 = remove, 0 = sidevote */
    amount SMALLINT NOT NULL,
    /* Metadata - Foreign key */
    nick_id BIGINT DEFAULT NULL,
    chan_id BIGINT DEFAULT NULL
);

/* Reaction vote audit table */
CREATE TABLE reacji_votes (
    id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    voted_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    by_whom_name TEXT NOT NULL,
    /* Type of action (ie add a reacji or remove) 1 = add, -1 = remove */
    action SMALLINT NOT NULL,
    /* Foreign Key */
    reacji_message_id BIGINT NOT NULL,
    /* 1 = add, -1 = remove, 0 = sidevote */
    amount SMALLINT NOT NULL,
    /* Metadata - Foreign key */
    nick_id BIGINT NOT NULL
);

/* Audit metadata tables */
CREATE TABLE reacji_message (
    id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    /* Slack timestamp, we do not touch it hence stored as TEXT */
    ts TEXT NOT NULL,
    chan_id BIGINT NOT NULL,
    nick_id BIGINT NOT NULL,
    message TEXT NOT NULL,
    UNIQUE (ts, chan_id)
);
CREATE TABLE nick_metadata (
    id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    cleaned_nick TEXT NOT NULL,
    full_name TEXT NOT NULL,
    username TEXT NOT NULL,
    hostmask TEXT NOT NULL,
    UNIQUE (cleaned_nick, full_name, username, hostmask)
);
CREATE TABLE chan_metadata (
    id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    channel TEXT NOT NULL,
    UNIQUE (channel)
);

/* Summarized tables for querying
 * NOTE: special case the name column- sqlite supports nocase and unlimited size
 * for this, but postgres doesn't (at least when its not unique)... So that means we
 * workaround it via the unique index with md5(lower(name)) below.
 *
 * Does mean when we are querying for it we will need to do a md5(lower(name)) == md5(lower(name))
 */
CREATE TABLE karma_received_count (
    id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    name TEXT NOT NULL,
    up BIGINT NOT NULL DEFAULT 0,
    down BIGINT NOT NULL DEFAULT 0,
    side BIGINT NOT NULL DEFAULT 0
);
CREATE UNIQUE INDEX ON karma_received_count (md5(lower(name)));

CREATE TABLE karma_given_count (
    id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
    name TEXT NOT NULL,
    up BIGINT NOT NULL DEFAULT 0,
    down BIGINT NOT NULL DEFAULT 0,
    side BIGINT NOT NULL DEFAULT 0
);
CREATE UNIQUE INDEX ON karma_given_count (md5(lower(name)));