CREATE TABLE chan_status (
	id INTEGER PRIMARY KEY NOT NULL,
	channel_id VARCHAR NOT NULL,
	channel_name VARCHAR NOT NULL,

	/* Outcome
	 * 0 = To-do
	 * 1 = Done/confirmed
	 * 2?? = Failed
	 * 	00 = Generic Failure
	 */
	outcome INTEGER NOT NULL,
	UNIQUE (channel_id)
);
