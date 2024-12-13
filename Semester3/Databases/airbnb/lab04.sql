

CREATE VIEW vw_PropertyDetails AS
SELECT property_id, title, property_type, price_per_night
FROM Properties;
GO

CREATE VIEW vw_BookingDetails AS
SELECT booking_id, property_id, guest_id, start_date, end_date, total_price
FROM Bookings;
GO

CREATE VIEW vw_UserDetails AS
SELECT user_id, first_name, last_name, email, phone, user_type
FROM Users;
GO

-- booking summary per user
CREATE VIEW vw_UserBookingSummary AS
SELECT
    u.user_id,
    u.first_name,
    u.last_name,
    COUNT(b.booking_id) AS total_bookings,
    SUM(b.total_price) AS total_booking_price,
    MIN(p.price_per_night) AS min_property_price,
    MAX(p.price_per_night) AS max_property_price
FROM
    Users u
JOIN
    Bookings b ON u.user_id = b.guest_id
JOIN
    Properties p ON b.property_id = p.property_id
GROUP BY
    u.user_id,
    u.first_name,
    u.last_name;
GO

-- property summary per user
CREATE VIEW vw_UserPropertySummary AS
SELECT
    u.user_id,
    u.first_name,
    u.last_name,
    COUNT(p.property_id) AS total_properties,
    MIN(p.price_per_night) AS min_property_price,
    MAX(p.price_per_night) AS max_property_price
FROM
    Users u
JOIN
    Properties p ON u.user_id = p.host_id
GROUP BY
    u.user_id,
    u.first_name,
    u.last_name;
GO

CREATE VIEW vw_PorpertyBookingDetails AS
SELECT p.property_id, p.title, p.property_type, p.price_per_night, b.booking_id, b.guest_id, b.start_date, b.end_date, b.total_price
FROM Properties p
JOIN Bookings b
ON p.property_id = b.property_id;
GO




CREATE PROCEDURE sp_insertDummyProperties
    @numProperties INT
AS
BEGIN
    DECLARE @i INT = 1;

    WHILE @i <= @numProperties
    BEGIN
        INSERT INTO Properties (host_id, title, address, city, state, country, zipcode, property_type, price_per_night, created_at, description)
        VALUES (1, 'Dummy Property ' + CAST(@i AS VARCHAR(10)), '1234 Elm St', 'Springfield', 'IL', 'USA', '62701', 'Apartment', 100.00, GETDATE(), 'This is a dummy property');

        SET @i = @i + 1;
    END
END;
GO

CREATE PROCEDURE sp_removeDummyProperties
AS
BEGIN
    DELETE FROM Properties
    WHERE title LIKE 'Dummy Property %';
END;
GO


CREATE PROCEDURE sp_insertDummyBookings
    @numBookings INT
AS
BEGIN
    DECLARE @i INT = 1;

    WHILE @i <= @numBookings
    BEGIN
        INSERT INTO Bookings (property_id, guest_id, start_date, end_date, total_price, status)
        VALUES (1, 1, '1999-01-01', '2024-12-12', 700.00, 'confirmed');

        SET @i = @i + 1;
    END
END;
GO

CREATE PROCEDURE sp_removeDummyBookings
AS
BEGIN
    DELETE FROM Bookings
    WHERE start_date = '1999-01-01';
END;
GO


CREATE PROCEDURE sp_insertDummyUsers
    @numUsers INT
AS
BEGIN
    DECLARE @i INT = 1;
    DECLARE @maxUserId INT;

    -- Get the highest user_id
    SELECT @maxUserId = ISNULL(MAX(user_id), 0) FROM Users;

    WHILE @i <= @numUsers
    BEGIN
        SET @maxUserId = @maxUserId + 1;

        INSERT INTO Users (first_name, last_name, email, password, phone, user_type, created_at)
        VALUES ('Dummy', 'User ' + CAST(@maxUserId AS VARCHAR(10)), 'dummyuser' + CAST(@maxUserId AS VARCHAR(10)) + '@gmail.com', 'password', '123-456-7890', 'host', GETDATE());

        SET @i = @i + 1;
    END
END;
GO

drop procedure sp_insertDummyUsers;

CREATE PROCEDURE sp_removeDummyUsers
AS
BEGIN
    DELETE FROM Users
    WHERE email LIKE 'dummyuser%';
END;
GO



EXEC sp_insertDummyProperties 10;
EXEC sp_removeDummyProperties;

EXEC sp_insertDummyBookings 10;
EXEC sp_removeDummyBookings;

EXEC sp_insertDummyUsers 10;
EXEC sp_removeDummyUsers;




CREATE OR ALTER PROCEDURE sp_runTest
    @TestName VARCHAR(50)
AS
BEGIN
    SET NOCOUNT ON;

    DECLARE @TestID INT = (SELECT TestID FROM Tests WHERE Name = @TestName);
    IF @TestID IS NULL RETURN;

    DECLARE @TestRunID INT;
    DECLARE @StartTime DATETIME = GETDATE();
    INSERT INTO TestRuns (Description, StartAt)
    VALUES (@TestName, @StartTime);
    SET @TestRunID = SCOPE_IDENTITY();

    -- Deleting data from test tables in the specified order
    DECLARE @TableID INT, @Position INT;
    DECLARE table_cursor CURSOR FOR
    SELECT TableID, Position
    FROM TestTables
    WHERE TestID = @TestID
    ORDER BY Position;

    OPEN table_cursor;
    FETCH NEXT FROM table_cursor INTO @TableID, @Position;

    WHILE @@FETCH_STATUS = 0
    BEGIN
        IF @TableID = (SELECT TableID FROM Tables WHERE Name = 'Properties')
            EXEC sp_removeDummyProperties;
        ELSE IF @TableID = (SELECT TableID FROM Tables WHERE Name = 'Bookings')
            EXEC sp_removeDummyBookings;
        ELSE IF @TableID = (SELECT TableID FROM Tables WHERE Name = 'Users')
            EXEC sp_removeDummyUsers;

        FETCH NEXT FROM table_cursor INTO @TableID, @Position;
    END

    CLOSE table_cursor;
    DEALLOCATE table_cursor;

    -- Inserting data into test tables in reverse deletion order
    DECLARE @NoOfRows INT, @InsertStart DATETIME, @InsertEnd DATETIME;
    DECLARE reverse_cursor CURSOR FOR
    SELECT TableID, NoOfRows
    FROM TestTables
    WHERE TestID = @TestID
    ORDER BY Position DESC;

    OPEN reverse_cursor;
    FETCH NEXT FROM reverse_cursor INTO @TableID, @NoOfRows;

    WHILE @@FETCH_STATUS = 0
    BEGIN
        SET @InsertStart = GETDATE();

        IF @TableID = (SELECT TableID FROM Tables WHERE Name = 'Properties')
            EXEC sp_insertDummyProperties @NoOfRows;
        ELSE IF @TableID = (SELECT TableID FROM Tables WHERE Name = 'Bookings')
            EXEC sp_insertDummyBookings @NoOfRows;
        ELSE IF @TableID = (SELECT TableID FROM Tables WHERE Name = 'Users')
            EXEC sp_insertDummyUsers @NoOfRows;

        SET @InsertEnd = GETDATE();

        INSERT INTO TestRunTables (TestRunID, TableID, StartAt, EndAt)
        VALUES (@TestRunID, @TableID, @InsertStart, @InsertEnd);

        FETCH NEXT FROM reverse_cursor INTO @TableID, @NoOfRows;
    END

    CLOSE reverse_cursor;
    DEALLOCATE reverse_cursor;

    -- Evaluating views
    DECLARE @ViewID INT;
    DECLARE view_cursor CURSOR FOR
    SELECT ViewID
    FROM TestViews
    WHERE TestID = @TestID;

    OPEN view_cursor;
    FETCH NEXT FROM view_cursor INTO @ViewID;

    WHILE @@FETCH_STATUS = 0
    BEGIN
        DECLARE @ViewStart DATETIME = GETDATE();

        IF @ViewID = (SELECT ViewID FROM Views WHERE Name = 'vw_PropertyDetails')
            SELECT * FROM vw_PropertyDetails;
        ELSE IF @ViewID = (SELECT ViewID FROM Views WHERE Name = 'vw_BookingDetails')
            SELECT * FROM vw_BookingDetails;
        ELSE IF @ViewID = (SELECT ViewID FROM Views WHERE Name = 'vw_UserDetails')
            SELECT * FROM vw_UserDetails;

        INSERT INTO TestRunViews (TestRunID, ViewID, StartAt, EndAt)
        VALUES (@TestRunID, @ViewID, @ViewStart, GETDATE());

        FETCH NEXT FROM view_cursor INTO @ViewID;
    END

    CLOSE view_cursor;
    DEALLOCATE view_cursor;

    -- Update the end time of the test run
    UPDATE TestRuns
    SET EndAt = GETDATE()
    WHERE TestRunID = @TestRunID;
END;
GO



-- Tables and views that are being tested
-- Insert data into Tables
INSERT INTO Tables (Name) VALUES
    ('Users'),
    ('Properties'),
    ('Bookings');

-- Insert data into Views
INSERT INTO Views (Name) VALUES
    ('vw_PropertyDetails'),
    ('vw_BookingDetails'),
    ('vw_UserDetails');



-- Test 1
INSERT INTO Tests (Name) VALUES
    ('Test1');

-- Declare variables for table and test IDs
DECLARE @UsersID INT = (SELECT TableID FROM Tables WHERE Name = 'Users');
DECLARE @PropertiesID INT = (SELECT TableID FROM Tables WHERE Name = 'Properties');
DECLARE @BookingsID INT = (SELECT TableID FROM Tables WHERE Name = 'Bookings');
DECLARE @TestID INT = (SELECT TestID FROM Tests WHERE Name = 'Test1');

-- Insert data into TestTables
INSERT INTO TestTables (TestID, TableID, NoOfRows, Position) VALUES
    (@TestID, @BookingsID, 10, 1),
    (@TestID, @PropertiesID, 10, 2),
    (@TestID, @UsersID, 10, 3);

-- Insert data into TestViews
INSERT INTO TestViews (TestID, ViewID)
SELECT @TestID, ViewID FROM Views;



-- Test 2
INSERT INTO Tests (Name) VALUES
    ('Test2');

DECLARE @UsersID INT = (SELECT TableID FROM Tables WHERE Name = 'Users');
DECLARE @PropertiesID INT = (SELECT TableID FROM Tables WHERE Name = 'Properties');
DECLARE @BookingsID INT = (SELECT TableID FROM Tables WHERE Name = 'Bookings');
DECLARE @Test2ID INT = (SELECT TestID FROM Tests WHERE Name = 'Test2');

INSERT INTO TestTables (TestID, TableID, NoOfRows, Position) VALUES
    (@Test2ID, @BookingsID, 1000, 1),
    (@Test2ID, @PropertiesID, 1000, 2),
    (@Test2ID, @UsersID, 1000, 3);

INSERT INTO TestViews (TestID, ViewID)
SELECT @Test2ID, ViewID FROM Views;



EXEC sp_runTest 'Test1';
EXEC sp_runTest 'Test2';


