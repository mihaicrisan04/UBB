-- Transaction 2: Update the record
BEGIN TRANSACTION;

UPDATE Users
SET first_name = 'NonRepeatableReadTest'
WHERE user_id = 1;

COMMIT TRANSACTION;