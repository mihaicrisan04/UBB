-- Transaction 1: Update a record and wait for 10 seconds
BEGIN TRANSACTION;
UPDATE Users
SET first_name = 'DirtyReadTest'
WHERE user_id = 1;

-- Wait for 10 seconds to simulate delay
WAITFOR DELAY '00:00:10';

-- Rollback the transaction
ROLLBACK TRANSACTION;