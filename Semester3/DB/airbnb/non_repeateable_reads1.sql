-- Transaction 1: Read the same record twice
BEGIN TRANSACTION;
SET TRANSACTION ISOLATION LEVEL READ COMMITTED;

-- First read
SELECT first_name
FROM Users
WHERE user_id = 1;

-- Wait for 10 seconds to allow another transaction to modify the record
WAITFOR DELAY '00:00:10';

-- Second read
SELECT first_name
FROM Users
WHERE user_id = 1;

COMMIT TRANSACTION;
