CREATE OR REPLACE FUNCTION insert_karma_count() RETURNS trigger AS $insert_karma_count$
    DECLARE col text;
    BEGIN
        -- Insert if these records doesn't already exist
        INSERT INTO karma_given_count (name) VALUES (NEW.by_whom_name) ON CONFLICT (name) DO NOTHING;
        INSERT INTO karma_received_count (name) VALUES (NEW.for_what_name) ON CONFLICT (name) DO NOTHING;

        -- Handle which column to update depending on the amount
        -- 1 = upvote, -1 = downvote, 0 = sidevote
        if NEW.amount = 0 THEN col := 'side';
        ELSIF NEW.amount = 1 THEN col := 'up';
        ELSIF NEW.amount = -1 THEN col := 'down';
        END IF;

        -- Due to the md5(lower(name)) index, we need to query like this here
        EXECUTE format('UPDATE karma_given_count SET %I = %I + 1 WHERE md5(lower(name)) = md5(lower($1))', col, col) USING NEW.by_whom_name;
        EXECUTE format('UPDATE karma_received_count SET %I = %I + 1 WHERE md5(lower(name)) = md5(lower($1))', col, col) USING NEW.for_what_name;

        RETURN NULL; -- Result is ignored since its an AFTER trigger
    END;
$insert_karma_count$ LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION delete_karma_count() RETURNS trigger AS $delete_karma_count$
    DECLARE col text;
    BEGIN
        -- Handle which column to update depending on the amount
        -- 1 = upvote, -1 = downvote, 0 = sidevote
        if OLD.amount = 0 THEN col := 'side';
        ELSIF OLD.amount = 1 THEN col := 'up';
        ELSIF OLD.amount = -1 THEN col := 'down';
        END IF;

        -- Due to the md5(lower(name)) index, we need to query like this here
        EXECUTE format('UPDATE karma_given_count SET %I = %I - 1 WHERE md5(lower(name)) = md5(lower($1))', col, col) USING OLD.by_whom_name;
        EXECUTE format('UPDATE karma_received_count SET %I = %I - 1 WHERE md5(lower(name)) = md5(lower($1))', col, col) USING OLD.for_what_name;

        RETURN NULL; -- Result is ignored since its an AFTER trigger
    END;
$delete_karma_count$ LANGUAGE PLPGSQL;

-- Setup vote table triggers
-- TODO: can we merge the insert+delete function somehow?
CREATE OR REPLACE TRIGGER insert_karma_count AFTER INSERT ON votes FOR EACH ROW EXECUTE FUNCTION insert_karma_count();
CREATE OR REPLACE TRIGGER delete_karma_count AFTER DELETE ON votes FOR EACH ROW EXECUTE FUNCTION delete_karma_count();


CREATE OR REPLACE FUNCTION insert_reacji_karma_count() RETURNS trigger AS $insert_reacji_karma_count$
    DECLARE col text;
    DECLARE var_message text;
    DECLARE var_for_what_name text;
    BEGIN
        -- Grab the message content and the for_what_name target for all of the later queries
        SELECT message INTO var_message FROM reacji_message WHERE id = NEW.reacji_message_id;
        SELECT cleaned_nick INTO var_for_what_name FROM nick_metadata
            INNER JOIN reacji_message ON nick_metadata.id = reacji_message.nick_id
            WHERE reacji_message.id = NEW.reacji_message_id;

        -- Insert if these records doesn't already exist
        -- Two entry in karma_received_count, one for karma on the message, one for whom sent the message
        INSERT INTO karma_given_count (name) VALUES (NEW.by_whom_name) ON CONFLICT (name) DO NOTHING;
        INSERT INTO karma_received_count (name) VALUES (var_message) ON CONFLICT (name) DO NOTHING;
        INSERT INTO karma_received_count (name) VALUES (var_for_what_name) ON CONFLICT (name) DO NOTHING;

        -- Handle which column to update depending on the amount
        -- 1 = upvote, -1 = downvote, 0 = sidevote
        -- Be clever because action: 1 = add, -1 = remove so we can just add action here
        if NEW.amount = 0 THEN col := 'side';
        ELSIF NEW.amount = 1 THEN col := 'up';
        ELSIF NEW.amount = -1 THEN col := 'down';
        END IF;

        -- Due to the md5(lower(name)) index, we need to query like this here
        EXECUTE format('UPDATE karma_given_count SET %I = %I + $1 WHERE md5(lower(name)) = md5(lower($2))', col, col) USING NEW.action, NEW.by_whom_name;
        EXECUTE format('UPDATE karma_received_count SET %I = %I + $1 WHERE md5(lower(name)) = md5(lower($2))', col, col) USING NEW.action, var_message;
        EXECUTE format('UPDATE karma_received_count SET %I = %I + $1 WHERE md5(lower(name)) = md5(lower($2))', col, col) USING NEW.action, var_for_what_name;

        RETURN NULL; -- Result is ignored since its an AFTER trigger
    END;
$insert_reacji_karma_count$ LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION delete_reacji_karma_count() RETURNS trigger AS $delete_reacji_karma_count$
    DECLARE col text;
    DECLARE var_message text;
    DECLARE var_for_what_name text;
    BEGIN
        -- Grab the message content and the for_what_name target for all of the later queries
        SELECT message INTO var_message FROM reacji_message WHERE id = OLD.reacji_message_id;
        SELECT cleaned_nick INTO var_for_what_name FROM nick_metadata
            INNER JOIN reacji_message ON nick_metadata.id = reacji_message.nick_id
            WHERE reacji_message.id = OLD.reacji_message_id;

        -- Handle which column to update depending on the amount
        -- 1 = upvote, -1 = downvote, 0 = sidevote
        -- Be clever because action: 1 = add, -1 = remove so we can just sub action here
        if OLD.amount = 0 THEN col := 'side';
        ELSIF OLD.amount = 1 THEN col := 'up';
        ELSIF OLD.amount = -1 THEN col := 'down';
        END IF;

        -- Due to the md5(lower(name)) index, we need to query like this here
        EXECUTE format('UPDATE karma_given_count SET %I = %I - $1 WHERE md5(lower(name)) = md5(lower($2))', col, col) USING OLD.action, OLD.by_whom_name;
        EXECUTE format('UPDATE karma_received_count SET %I = %I - $1 WHERE md5(lower(name)) = md5(lower($2))', col, col) USING OLD.action, var_message;
        EXECUTE format('UPDATE karma_received_count SET %I = %I - $1 WHERE md5(lower(name)) = md5(lower($2))', col, col) USING OLD.action, var_for_what_name;

        RETURN NULL; -- Result is ignored since its an AFTER trigger
    END;
$delete_reacji_karma_count$ LANGUAGE PLPGSQL;

-- Setup reacji_votes table triggers
CREATE OR REPLACE TRIGGER insert_reacji_karma_count AFTER INSERT ON reacji_votes FOR EACH ROW EXECUTE FUNCTION insert_reacji_karma_count();
CREATE OR REPLACE TRIGGER delete_reacji_karma_count AFTER DELETE ON reacji_votes FOR EACH ROW EXECUTE FUNCTION delete_reacji_karma_count();
