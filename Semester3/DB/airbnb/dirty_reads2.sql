-- Transaction 2: Read uncommitted data
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
SELECT first_name
FROM Users
WHERE user_id = 1;