-- Transaction 2: Read and attempt to update the same record
SET TRANSACTION ISOLATION LEVEL SNAPSHOT;
BEGIN TRANSACTION;

-- Read the record
SELECT first_name, last_name
FROM Users
WHERE user_id = 1;

-- Simulate some processing time
WAITFOR DELAY '00:00:15';

-- Attempt to update the record
UPDATE Users
SET first_name = 'OptimisticUpdate2'
WHERE user_id = 1;

COMMIT TRANSACTION;