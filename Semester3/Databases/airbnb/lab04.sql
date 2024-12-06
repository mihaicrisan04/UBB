

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