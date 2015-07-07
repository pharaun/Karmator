/* Create the simple state table */
CREATE TABLE simple_state (
    module_name VARCHAR NOT NULL,
    state_key VARCHAR NOT NULL,
    state BLOB NOT NULL,
    PRIMARY KEY (module_name, state_key)
);
