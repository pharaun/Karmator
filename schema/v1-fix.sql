CREATE OR REPLACE FUNCTION fix_duplicate_reacji_message() RETURNS void AS $$
	DECLARE r RECORD;
	DECLARE rm reacji_message.id%TYPE;
    BEGIN
		-- Get list of duplicate
		for r in
			select id as rm_id
			from reacji_message
			where id in (
				select id from reacji_message group by id having count(*) > 1
			) and ts >= '1733745181.248359'
		loop
			update reacji_message
			set id = nextval(pg_get_serial_sequence('reacji_message','id'))
			where id = r.rm_id AND ts >= '1733745181.248359' RETURNING ID into rm;

			RAISE NOTICE 'reacji_message - old: % - new: %', r.rm_id, rm;

			-- Now update reacji votes
			update reacji_votes
			set reacji_message_id = rm
			where reacji_message_id = r.rm_id
			and id >= 140971;
		end loop;
    END;
$$ LANGUAGE PLPGSQL;
