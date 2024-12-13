-- Lab 4

-- Views
DELETE FROM Users;
-- view with a SELECT statement operating on one table
-- view of all users
CREATE VIEW UsersView AS
    SELECT * FROM Users;

-- view with a SELECT statement operating on at least 2 tables
-- views each account and its transactions
CREATE VIEW AccountTransactionsView AS
    SELECT a.account_id, a.account_name, t.transaction_id, t.transaction_date, t.amount
    FROM Accounts a
    JOIN Transactions t ON a.account_id = t.account_id;


-- view with a SELECT statement that has a GROUP BY clause and operates on at least 2 tables
-- view of all accounts and their total transaction amounts
CREATE VIEW AccountTotalsView AS
    SELECT a.account_id, a.account_name, SUM(t.amount) AS total
    FROM Accounts a
    JOIN Transactions t ON a.account_id = t.account_id
    GROUP BY a.account_id, a.account_name;


-- procedures to insert k values into tables

-- insert k users
CREATE OR ALTER PROCEDURE InsertUsers
    @k INT
AS
BEGIN
    DECLARE @i INT = 1;
    DECLARE @last_id INT;

    -- Get the last user_id
    SELECT @last_id = ISNULL(MAX(user_id), 0) FROM Users;

    WHILE @i <= @k
    BEGIN
        INSERT INTO Users (username, email, password_hash)
        VALUES ('usr. ' + CAST(@last_id + @i AS VARCHAR), 'emailname' + CAST(@last_id + @i AS VARCHAR) + '@gmail.com', 'strong_pass' + CAST(@last_id + @i AS VARCHAR));
        SET @i = @i + 1;
    END;
END;
GO

EXEC InsertUsers 50;
SELECT * FROM Users;
DELETE FROM Users;

-- insert k accounts
CREATE OR ALTER PROCEDURE InsertAccounts
    @k INT
AS
BEGIN
    DECLARE @i INT = 1;
    DECLARE @max_user_id INT;

    -- Get the maximum user_id from the Users table
    SELECT @max_user_id = MAX(user_id) FROM Users;

    WHILE @i <= @k
    BEGIN
        -- Ensure the user_id exists in the Users table
        IF @i <= @max_user_id
        BEGIN
            INSERT INTO Accounts (user_id, account_name, type, balance)
            VALUES (@i, 'account' + CAST(@i AS VARCHAR), 'Checking', 1000.00);
        END
        SET @i = @i + 1;
    END;
END;
GO

EXEC InsertAccounts 5;
SELECT * FROM Accounts;

-- insert k transactions
CREATE PROCEDURE InsertTransactions
    @k INT
AS
BEGIN
    DECLARE @i INT = 1;
    WHILE @i <= @k
    BEGIN
        INSERT INTO Transactions (account_id, category_id, transaction_name, amount, frequency)
        VALUES (@i, @i, 'transaction' + CAST(@i AS VARCHAR), 100.00, 'One-Time');
        SET @i = @i + 1;
    END;
END;
GO

EXEC InsertTransactions 5;
SELECT * FROM Transactions;





-- Create procedures


BEGIN
    -- Clear existing data
    DELETE FROM TestRunViews;
    DELETE FROM TestRunTables;
    DELETE FROM TestRuns;
    DELETE FROM TestViews;
    DELETE FROM TestTables;
    DELETE FROM Tests;
    DELETE FROM Tables;
    DELETE FROM Views;

    -- Insert new data into Tables
    INSERT INTO Tables (Name) VALUES
        ('Users'),
        ('Accounts'),
        ('Transactions');

    -- Insert new data into Views
    INSERT INTO Views (Name) VALUES
        ('UsersView'),
        ('AccountTransactionsView'),
        ('AccountTotalsView');

    -- Insert new data into Tests
    INSERT INTO Tests (Name) VALUES
        ('delete_table_100'),
        ('insert_table_100'),
        ('select_view');

    -- Declare variables for table and test IDs
    DECLARE @UsersID INT = (SELECT TableID FROM Tables WHERE Name = 'Users');
    DECLARE @AccountsID INT = (SELECT TableID FROM Tables WHERE Name = 'Accounts');
    DECLARE @TransactionsID INT = (SELECT TableID FROM Tables WHERE Name = 'Transactions');
    DECLARE @DeleteTestID INT = (SELECT TestID FROM Tests WHERE Name = 'delete_table_100');
    DECLARE @InsertTestID INT = (SELECT TestID FROM Tests WHERE Name = 'insert_table_100');
    DECLARE @SelectTestID INT = (SELECT TestID FROM Tests WHERE Name = 'select_view');

    -- Insert data into TestTables
    INSERT INTO TestTables (TestID, TableID, NoOfRows, Position) VALUES
        (@DeleteTestID, @UsersID, 100, 1),
        (@DeleteTestID, @AccountsID, 100, 2),
        (@DeleteTestID, @TransactionsID, 100, 3),
        (@InsertTestID, @UsersID, 100, 1),
        (@InsertTestID, @AccountsID, 100, 2),
        (@InsertTestID, @TransactionsID, 100, 3);

    -- Insert data into TestViews
    INSERT INTO TestViews (TestID, ViewID)
    SELECT @SelectTestID, ViewID FROM Views;
END;
GO

-- Create or alter the RunTest procedure
CREATE OR ALTER PROCEDURE RunTest
    @TestName VARCHAR(50),
    @ViewName varchar(50) = NULL,
    @InsertName varchar(50) = NULL,
    @DeleteName varchar(50) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    DECLARE @TestID INT = (SELECT TestID FROM Tests WHERE Name = @TestName);
    IF @TestID IS NULL RETURN;

    DECLARE @TestRunID INT;
    DECLARE @StartTime DATETIME = GETDATE();
    INSERT INTO TestRuns (Description, StartAt)
    VALUES (@TestID, @StartTime);
    SET @TestRunID = SCOPE_IDENTITY();

    IF @TestName = 'delete_table_100'
    BEGIN
        DECLARE @DeleteStart DATETIME = GETDATE();

        -- Delete related records in AccountGoals first
        DELETE FROM AccountGoals WHERE account_id IN (SELECT account_id FROM Accounts);
        DELETE FROM Investments WHERE account_id IN (SELECT account_id FROM Accounts);
        DELETE FROM Bills WHERE account_id IN (SELECT account_id FROM Accounts);
        DELETE FROM Reports WHERE user_id IN (SELECT user_id FROM Users);
        DELETE FROM Budgets WHERE user_id IN (SELECT user_id FROM Users);
        DELETE FROM Goals WHERE user_id IN (SELECT user_id FROM Users);
        DELETE FROM Notifications WHERE user_id IN (SELECT user_id FROM Users);

        if @DeleteName = 'Transactions' OR @DeleteName IS NULL
            DELETE FROM Transactions;
        if @DeleteName = 'Accounts' OR @DeleteName IS NULL
            DELETE FROM Accounts;
        if @DeleteName = 'Users' OR @DeleteName IS NULL
            DELETE FROM Users;

        INSERT INTO TestRunTables (TestRunID, TableID, StartAt, EndAt)
        SELECT
            @TestRunID,
            TableID,
            @DeleteStart,
            GETDATE()
        FROM TestTables
        WHERE TestID = @TestID;
    END

    IF @TestName = 'insert_table_100'
    BEGIN
        DECLARE @InsertStart DATETIME = GETDATE();
        DECLARE @RowCount INT;

        SELECT @RowCount = NoOfRows
        FROM TestTables tt
        JOIN Tables t ON tt.TableID = t.TableID
        WHERE t.Name = 'Users' AND tt.TestID = @TestID;

        BEGIN TRY
    IF @InsertName = 'Users' OR @InsertName IS NULL
        EXEC InsertUsers @RowCount;
    IF @InsertName = 'Accounts' OR @InsertName IS NULL
        EXEC InsertAccounts @RowCount;
    IF @InsertName = 'Transactions' OR @InsertName IS NULL
        EXEC InsertTransactions @RowCount;
END TRY
BEGIN CATCH
    -- Handle the error
    DECLARE @ErrorMessage NVARCHAR(4000);
    DECLARE @ErrorSeverity INT;
    DECLARE @ErrorState INT;

    SELECT
        @ErrorMessage = ERROR_MESSAGE(),
        @ErrorSeverity = ERROR_SEVERITY(),
        @ErrorState = ERROR_STATE();

    -- Print the error message
    PRINT 'Error occurred: ' + @ErrorMessage;

    -- Optionally, you can re-throw the error
    RAISERROR (@ErrorMessage, @ErrorSeverity, @ErrorState);
END CATCH;

        INSERT INTO TestRunTables (TestRunID, TableID, StartAt)
        SELECT
            @TestRunID,
            TableID,
            DATEDIFF(MILLISECOND, @InsertStart, GETDATE()) / 1000.0
        FROM TestTables
        WHERE TestID = @TestID;
    END

    IF @TestName = 'select_view'
BEGIN
    DECLARE @ViewStart DATETIME = GETDATE();

    IF @TestName = 'UsersView' OR @ViewName IS NULL
    BEGIN
        SELECT * FROM UsersView;
    END

    IF @ViewName = 'AccountTransactionsView' OR @ViewName IS NULL
    BEGIN
        SELECT * FROM AccountTransactionsView;
    END

    IF @ViewName = 'AccountTotalsView' OR @ViewName IS NULL
    BEGIN
        SELECT * FROM AccountTotalsView;
    END

    INSERT INTO TestRunViews (TestRunID, ViewID, StartAt, EndAt)
    SELECT
        @TestRunID,
        ViewID,
        @ViewStart,
        GETDATE()
    FROM TestViews
    WHERE TestID = @TestID
    AND (@ViewName IS NULL OR EXISTS (
        SELECT 1 FROM Views v
        WHERE v.ViewID = TestViews.ViewID
        AND v.Name = @ViewName
    ));
END

    UPDATE TestRuns
    SET EndAt = GETDATE()
    WHERE TestRunID = @TestRunID;
END;
GO

-- Execute the tests
EXEC RunTest 'delete_table_100', 'Users';
EXEC RunTest 'delete_table_100', 'Accounts';
EXEC RunTest 'delete_table_100', 'Transactions';

EXEC RunTest 'insert_table_100', 'Users';
EXEC RunTest 'insert_table_100', 'Accounts';
EXEC RunTest 'insert_table_100', 'Transactions';

EXEC RunTest 'select_view', 'UsersView';
EXEC RunTest 'select_view', 'AccountTransactionsView';
EXEC RunTest 'select_view', 'AccountTotalsView';

SELECT * FROM Users;
-- Display the results
SELECT
    TestRunID,
    StartAt,
    DATEDIFF(MILLISECOND, StartAt, GETDATE()) AS Duration_ms
FROM TestRuns;

SELECT
    tr.TestRunID,
    t.Name,
    DATEDIFF(MILLISECOND, trt.StartAt, GETDATE()) AS Duration_ms
FROM TestRunTables trt
JOIN TestRuns tr ON trt.TestRunID = tr.TestRunID
JOIN Tables t ON trt.TableID = t.TableID;