-- Transaction 1: Read and update a record
SET TRANSACTION ISOLATION LEVEL SNAPSHOT;
BEGIN TRANSACTION;

-- Read the record
SELECT first_name, last_name
FROM Users
WHERE user_id = 1;

-- Simulate some processing time
WAITFOR DELAY '00:00:10';

-- Update the record
UPDATE Users
SET first_name = 'OptimisticUpdate1'
WHERE user_id = 1;

COMMIT TRANSACTION;
