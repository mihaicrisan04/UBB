-- LogTable for auditing
CREATE TABLE LogTable (
    log_id INT PRIMARY KEY IDENTITY(1,1),
    log_time DATETIME DEFAULT GETDATE(),
    typeOperation VARCHAR(50),       -- e.g., 'begin', 'insert', 'update', 'delete', 'commit', 'rollback', 'error', 'info'
    procedure_name VARCHAR(100),    -- Name of the stored procedure or function
    tableOperation VARCHAR(100),     -- Name of the table being operated on
    message VARCHAR(MAX)
);
GO

-- Validation Functions

-- Validate User Type
CREATE FUNCTION uf_ValidateUserType (@user_type VARCHAR(10))
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF @user_type IN ('host', 'guest')
        SET @return = 1;
    RETURN @return;
END
GO

-- Validate Email Format (basic)
CREATE FUNCTION uf_ValidateEmailFormat (@email VARCHAR(255))
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF @email LIKE '%_@__%.__%' AND CHARINDEX(' ', @email) = 0 AND LEN(@email) > 5 
        SET @return = 1;
    RETURN @return;
END
GO

-- Validate Password Strength (basic: min 8 chars)
CREATE FUNCTION uf_ValidatePasswordStrength (@password VARCHAR(255))
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF LEN(@password) >= 8
        SET @return = 1;
    RETURN @return;
END
GO

-- Validate Phone Number (basic: not empty, can add more checks)
CREATE FUNCTION uf_ValidatePhoneNumber (@phone VARCHAR(20))
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF @phone IS NOT NULL AND LEN(@phone) > 0 -- Basic check, can be enhanced
        SET @return = 1;
    RETURN @return;
END
GO

-- Validate Name (generic for first_name, last_name, etc. - starts with uppercase, min length 2)
CREATE FUNCTION uf_ValidateName (@name VARCHAR(100))
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF LEN(@name) >= 2 AND LEFT(@name, 1) = UPPER(LEFT(@name, 1)) COLLATE Latin1_General_CS_AS
        SET @return = 1;
    RETURN @return;
END
GO

-- Validate Property Type
CREATE FUNCTION uf_ValidatePropertyType (@property_type VARCHAR(50))
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF @property_type IN ('apartment', 'house', 'villa')
        SET @return = 1;
    RETURN @return;
END
GO

-- Validate Price (must be positive)
CREATE FUNCTION uf_ValidatePrice (@price DECIMAL(10, 2))
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF @price > 0
        SET @return = 1;
    RETURN @return;
END
GO

-- Validate Rating (1 to 5)
CREATE FUNCTION uf_ValidateRating (@rating INT)
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF @rating BETWEEN 1 AND 5
        SET @return = 1;
    RETURN @return;
END
GO

-- Validate Booking Dates (start_date < end_date, start_date >= today)
CREATE FUNCTION uf_ValidateBookingDates (@start_date DATE, @end_date DATE)
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF @start_date < @end_date AND @start_date >= CAST(GETDATE() AS DATE)
        SET @return = 1;
    RETURN @return;
END
GO

-- Validate Generic Status (e.g., for Bookings or Payments)
CREATE FUNCTION uf_ValidateStatus (@status VARCHAR(20), @context VARCHAR(20))
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF @context = 'Booking' AND @status IN ('pending', 'confirmed', 'cancelled')
        SET @return = 1;
    ELSE IF @context = 'Payment' AND @status IN ('pending', 'completed', 'failed')
        SET @return = 1;
    RETURN @return;
END
GO

-- Validate a generic text field is not empty or just whitespace
CREATE FUNCTION uf_ValidateTextNotEmpty (@text varchar(max))
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF @text IS NOT NULL AND LEN(LTRIM(RTRIM(@text))) > 0
        SET @return = 1;
    RETURN @return;
END
GO

-- Validate a string field is not empty or just whitespace (for VARCHAR)
CREATE FUNCTION uf_ValidateStringNotEmpty (@text VARCHAR(MAX))
RETURNS INT AS
BEGIN
    DECLARE @return INT = 0;
    IF @text IS NOT NULL AND LEN(LTRIM(RTRIM(@text))) > 0
        SET @return = 1;
    RETURN @return;
END
GO

PRINT 'LogTable and Validation Functions created successfully.'
GO

----------------------------------------------------------------------------------------------------
-- Stored Procedure 1: Create a Booking and Add a Review
-- Analogue to AddRaceDriverResult
----------------------------------------------------------------------------------------------------
CREATE OR ALTER PROCEDURE CreateBookingAndAddReview
    @property_id INT,
    @guest_id INT,
    @start_date DATE,
    @end_date DATE,
    -- @total_price DECIMAL(10, 2), -- This will be calculated based on property price and duration
    @booking_status VARCHAR(20) = 'pending', -- Default to pending
    @review_rating INT,
    @review_comment TEXT
AS
BEGIN
    DECLARE @procedure_name VARCHAR(100) = 'CreateBookingAndAddReview';
    DECLARE @new_booking_id INT;
    DECLARE @property_price_per_night DECIMAL(10,2);
    DECLARE @calculated_total_price DECIMAL(10,2);
    DECLARE @duration_in_days INT;

    INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
    VALUES ('begin', @procedure_name, NULL, 'Procedure started');

    BEGIN TRY
        BEGIN TRANSACTION;

        -- Validate inputs
        IF NOT EXISTS (SELECT 1 FROM Properties WHERE property_id = @property_id)
        BEGIN
            RAISERROR('Invalid property_id: Property does not exist.', 16, 1);
        END
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('info', @procedure_name, 'Properties', 'Property ID validated: ' + CAST(@property_id AS VARCHAR));

        IF NOT EXISTS (SELECT 1 FROM Users WHERE user_id = @guest_id AND user_type = 'guest')
        BEGIN
            RAISERROR('Invalid guest_id: User is not a guest or does not exist.', 16, 1);
        END
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('info', @procedure_name, 'Users', 'Guest ID validated: ' + CAST(@guest_id AS VARCHAR));

        IF dbo.uf_ValidateBookingDates(@start_date, @end_date) = 0
        BEGIN
            RAISERROR('Invalid booking dates: Start date must be before end date and not in the past.', 16, 1);
        END
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('info', @procedure_name, 'Bookings', 'Booking dates validated.');

        IF dbo.uf_ValidateStatus(@booking_status, 'Booking') = 0
        BEGIN
            RAISERROR('Invalid booking status.', 16, 1);
        END
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('info', @procedure_name, 'Bookings', 'Booking status validated: ' + @booking_status);

        IF dbo.uf_ValidateRating(@review_rating) = 0
        BEGIN
            RAISERROR('Invalid review rating: Rating must be between 1 and 5.', 16, 1);
        END
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('info', @procedure_name, 'Reviews', 'Review rating validated: ' + CAST(@review_rating AS VARCHAR));

        IF dbo.uf_ValidateTextNotEmpty(@review_comment) = 0
        BEGIN
            RAISERROR('Invalid review comment: Comment cannot be empty.', 16, 1);
        END
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('info', @procedure_name, 'Reviews', 'Review comment validated.');

        -- Calculate total price
        SELECT @property_price_per_night = price_per_night FROM Properties WHERE property_id = @property_id;
        SET @duration_in_days = DATEDIFF(day, @start_date, @end_date);
        IF @duration_in_days <= 0 SET @duration_in_days = 1; -- Minimum 1 day charge if dates are same or invalid diff
        SET @calculated_total_price = @property_price_per_night * @duration_in_days;

        IF dbo.uf_ValidatePrice(@calculated_total_price) = 0 AND @calculated_total_price > 0 -- price can be 0 if duration is 0 but that is an error
        BEGIN
             RAISERROR('Calculated total price is not valid (must be > 0).', 16, 1);
        END
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('info', @procedure_name, 'Bookings', 'Total price calculated: ' + CAST(@calculated_total_price AS VARCHAR));

        -- Insert into Bookings
        INSERT INTO Bookings (property_id, guest_id, start_date, end_date, total_price, status)
        VALUES (@property_id, @guest_id, @start_date, @end_date, @calculated_total_price, @booking_status);
        
        SET @new_booking_id = SCOPE_IDENTITY();
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('insert', @procedure_name, 'Bookings', 'New booking created. ID: ' + CAST(@new_booking_id AS VARCHAR));

        -- Insert into Reviews
        INSERT INTO Reviews (property_id, guest_id, rating, comment, booking_id) -- Added booking_id to Reviews table schema
        VALUES (@property_id, @guest_id, @review_rating, @review_comment, @new_booking_id);

        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('insert', @procedure_name, 'Reviews', 'New review created for booking ID: ' + CAST(@new_booking_id AS VARCHAR));

        COMMIT TRANSACTION;
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('commit', @procedure_name, NULL, 'Transaction committed successfully.');

    END TRY
    BEGIN CATCH
        IF @@TRANCOUNT > 0
            ROLLBACK TRANSACTION;
        
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('error', @procedure_name, NULL, 'Error: ' + ERROR_MESSAGE());
        
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message)
        VALUES ('rollback', @procedure_name, NULL, 'Transaction rolled back due to error.');
        
        THROW;
    END CATCH
END
GO

PRINT 'Stored Procedure CreateBookingAndAddReview created successfully.'
GO

----------------------------------------------------------------------------------------------------
-- Stored Procedure 2: Register Host, Add Property, Register Guest, and Create Booking (Recoverable)
-- Analogue to AddRaceDriverResultRecoverable
----------------------------------------------------------------------------------------------------
CREATE OR ALTER PROCEDURE RegisterUsersPropertyAndBooking_Recoverable
    -- Host details
    @host_first_name VARCHAR(100),
    @host_last_name VARCHAR(100),
    @host_email VARCHAR(255),
    @host_password VARCHAR(255),
    @host_phone VARCHAR(20) = NULL,
    -- Property details
    @prop_title VARCHAR(255),
    @prop_description varchar(max),
    @prop_address VARCHAR(255),
    @prop_city VARCHAR(100),
    @prop_state VARCHAR(100) = NULL,
    @prop_country VARCHAR(100),
    @prop_zipcode VARCHAR(20) = NULL,
    @prop_property_type VARCHAR(50),
    @prop_price_per_night DECIMAL(10, 2),
    -- Guest details
    @guest_first_name VARCHAR(100),
    @guest_last_name VARCHAR(100),
    @guest_email VARCHAR(255),
    @guest_password VARCHAR(255),
    @guest_phone VARCHAR(20) = NULL,
    -- Booking details
    @book_start_date DATE,
    @book_end_date DATE,
    @book_status VARCHAR(20) = 'pending'
AS
BEGIN
    DECLARE @procedure_name VARCHAR(100) = 'RegisterUsersPropertyAndBooking_Recoverable';
    INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
    VALUES ('begin', @procedure_name, NULL, 'Procedure started');

    DECLARE @host_id INT, @property_id INT, @guest_id INT, @booking_id INT;
    DECLARE @host_status VARCHAR(255) = 'Host creation pending';
    DECLARE @property_status VARCHAR(255) = 'Property creation pending';
    DECLARE @guest_status VARCHAR(255) = 'Guest creation pending';
    DECLARE @booking_status_report VARCHAR(255) = 'Booking creation pending';
    DECLARE @calculated_total_price DECIMAL(10,2);
    DECLARE @duration_in_days INT;

    -- Step 1: Create or Find Host User
    BEGIN TRY
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
        VALUES ('info', @procedure_name, 'Users', 'Attempting to find or create host: ' + @host_email);

        IF dbo.uf_ValidateName(@host_first_name) = 0 RAISERROR('Invalid host first name.', 16, 1);
        IF dbo.uf_ValidateName(@host_last_name) = 0 RAISERROR('Invalid host last name.', 16, 1);
        IF dbo.uf_ValidateEmailFormat(@host_email) = 0 RAISERROR('Invalid host email format.', 16, 1);
        IF dbo.uf_ValidatePasswordStrength(@host_password) = 0 RAISERROR('Host password too weak.', 16, 1);
        IF @host_phone IS NOT NULL AND dbo.uf_ValidatePhoneNumber(@host_phone) = 0 RAISERROR('Invalid host phone number.', 16, 1);

        SELECT @host_id = user_id FROM Users WHERE email = @host_email AND user_type = 'host';

        IF @host_id IS NULL
        BEGIN
            BEGIN TRANSACTION HostCreation;
            INSERT INTO Users (first_name, last_name, email, password, phone, user_type)
            VALUES (@host_first_name, @host_last_name, @host_email, @host_password, @host_phone, 'host');
            SET @host_id = SCOPE_IDENTITY();
            COMMIT TRANSACTION HostCreation;
            set @host_status = 'Host created successfully. ID: ' + CAST(@host_id AS VARCHAR);
            INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
            VALUES ('insert', @procedure_name, 'Users', @host_status);
        END
        ELSE
        BEGIN
            set @host_status = 'Existing host found. ID: ' + CAST(@host_id AS VARCHAR);
            INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
            VALUES ('info', @procedure_name, 'Users', @host_status);
        END
    END TRY
    BEGIN CATCH
        IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION HostCreation;
        set @host_status = 'Host creation failed: ' + ERROR_MESSAGE();
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
        VALUES ('error', @procedure_name, 'Users', @host_status);
    END CATCH

    -- Step 2: Create Property (only if host was created/found)
    IF @host_id IS NOT NULL
    BEGIN
        BEGIN TRY
            INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
            VALUES ('info', @procedure_name, 'Properties', 'Attempting to create property: ' + @prop_title + ' for host ID: ' + CAST(@host_id AS VARCHAR));

            IF dbo.uf_ValidateStringNotEmpty(@prop_title) = 0 RAISERROR('Property title cannot be empty.', 16, 1);
            IF dbo.uf_ValidateTextNotEmpty(@prop_description) = 0 RAISERROR('Property description cannot be empty.', 16, 1);
            IF dbo.uf_ValidateStringNotEmpty(@prop_address) = 0 RAISERROR('Property address cannot be empty.', 16, 1);
            IF dbo.uf_ValidateStringNotEmpty(@prop_city) = 0 RAISERROR('Property city cannot be empty.', 16, 1);
            IF dbo.uf_ValidateStringNotEmpty(@prop_country) = 0 RAISERROR('Property country cannot be empty.', 16, 1);
            IF dbo.uf_ValidatePropertyType(@prop_property_type) = 0 RAISERROR('Invalid property type.', 16, 1);
            IF dbo.uf_ValidatePrice(@prop_price_per_night) = 0 RAISERROR('Invalid property price per night.', 16, 1);

            -- For simplicity, we'll always create a new property. Add duplicate checks if needed.
            BEGIN TRANSACTION PropertyCreation;
            INSERT INTO Properties (host_id, title, description, address, city, state, country, zipcode, property_type, price_per_night)
            VALUES (@host_id, @prop_title, @prop_description, @prop_address, @prop_city, @prop_state, @prop_country, @prop_zipcode, @prop_property_type, @prop_price_per_night);
            SET @property_id = SCOPE_IDENTITY();
            COMMIT TRANSACTION PropertyCreation;
            set @property_status = 'Property created successfully. ID: ' + CAST(@property_id AS VARCHAR);
            INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
            VALUES ('insert', @procedure_name, 'Properties', @property_status);
        END TRY
        BEGIN CATCH
            IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION PropertyCreation;
            set @property_status = 'Property creation failed: ' + ERROR_MESSAGE();
            INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
            VALUES ('error', @procedure_name, 'Properties', @property_status);
        END CATCH
    END
    ELSE
    BEGIN
        set @property_status = 'Property creation skipped: Host creation failed or host not found.';
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
        VALUES ('skip', @procedure_name, 'Properties', @property_status);
    END

    -- Step 3: Create or Find Guest User (only if property was created)
    IF @property_id IS NOT NULL
    BEGIN
        BEGIN TRY
            INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
            VALUES ('info', @procedure_name, 'Users', 'Attempting to find or create guest: ' + @guest_email);

            IF dbo.uf_ValidateName(@guest_first_name) = 0 RAISERROR('Invalid guest first name.', 16, 1);
            IF dbo.uf_ValidateName(@guest_last_name) = 0 RAISERROR('Invalid guest last name.', 16, 1);
            IF dbo.uf_ValidateEmailFormat(@guest_email) = 0 RAISERROR('Invalid guest email format.', 16, 1);
            IF dbo.uf_ValidatePasswordStrength(@guest_password) = 0 RAISERROR('Guest password too weak.', 16, 1);
            IF @guest_phone IS NOT NULL AND dbo.uf_ValidatePhoneNumber(@guest_phone) = 0 RAISERROR('Invalid guest phone number.', 16, 1);

            SELECT @guest_id = user_id FROM Users WHERE email = @guest_email AND user_type = 'guest';

            IF @guest_id IS NULL
            BEGIN
                BEGIN TRANSACTION GuestCreation;
                INSERT INTO Users (first_name, last_name, email, password, phone, user_type)
                VALUES (@guest_first_name, @guest_last_name, @guest_email, @guest_password, @guest_phone, 'guest');
                SET @guest_id = SCOPE_IDENTITY();
                COMMIT TRANSACTION GuestCreation;
                set @guest_status = 'Guest created successfully. ID: ' + CAST(@guest_id AS VARCHAR);
                INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
                VALUES ('insert', @procedure_name, 'Users', @guest_status);
            END
            ELSE
            BEGIN
                set @guest_status = 'Existing guest found. ID: ' + CAST(@guest_id AS VARCHAR);
                INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
                VALUES ('info', @procedure_name, 'Users', @guest_status);
            END
        END TRY
        BEGIN CATCH
            IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION GuestCreation;
            set @guest_status = 'Guest creation failed: ' + ERROR_MESSAGE();
            INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
            VALUES ('error', @procedure_name, 'Users', @guest_status);
        END CATCH
    END
    ELSE
    BEGIN
        set @guest_status = 'Guest creation skipped: Property creation failed.';
         INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
        VALUES ('skip', @procedure_name, 'Users', @guest_status);
    END

    -- Step 4: Create Booking (only if guest and property are available)
    IF @guest_id IS NOT NULL AND @property_id IS NOT NULL
    BEGIN
        BEGIN TRY
            INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
            VALUES ('info', @procedure_name, 'Bookings', 'Attempting to create booking for guest ID: ' + CAST(@guest_id AS VARCHAR) + ' at property ID: ' + CAST(@property_id AS VARCHAR));

            IF dbo.uf_ValidateBookingDates(@book_start_date, @book_end_date) = 0 RAISERROR('Invalid booking dates.', 16, 1);
            IF dbo.uf_ValidateStatus(@book_status, 'Booking') = 0 RAISERROR('Invalid booking status.', 16, 1);

            SET @duration_in_days = DATEDIFF(day, @book_start_date, @book_end_date);
            IF @duration_in_days <= 0 SET @duration_in_days = 1; -- Or handle as error
            
            SELECT @prop_price_per_night = price_per_night FROM Properties WHERE property_id = @property_id;
            SET @calculated_total_price = @prop_price_per_night * @duration_in_days;

            IF dbo.uf_ValidatePrice(@calculated_total_price) = 0 AND @calculated_total_price > 0 
            BEGIN
                RAISERROR('Calculated total price is not valid (must be > 0).', 16, 1);
            END

            BEGIN TRANSACTION BookingCreation;
            INSERT INTO Bookings (property_id, guest_id, start_date, end_date, total_price, status)
            VALUES (@property_id, @guest_id, @book_start_date, @book_end_date, @calculated_total_price, @book_status);
            SET @booking_id = SCOPE_IDENTITY();
            COMMIT TRANSACTION BookingCreation;
            set @booking_status_report = 'Booking created successfully. ID: ' + CAST(@booking_id AS VARCHAR);
            INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
            VALUES ('insert', @procedure_name, 'Bookings', @booking_status_report);
        END TRY
        BEGIN CATCH
            IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION BookingCreation;
            set @booking_status_report = 'Booking creation failed: ' + ERROR_MESSAGE();
            INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
            VALUES ('error', @procedure_name, 'Bookings', @booking_status_report);
        END CATCH
    END
    ELSE
    BEGIN
        set @booking_status_report = 'Booking creation skipped: Guest or Property not available.';
        INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
        VALUES ('skip', @procedure_name, 'Bookings', @booking_status_report);
    END

    -- Report final status
    SELECT 
        HostStatus = @host_status,
        PropertyStatus = @property_status,
        GuestStatus = @guest_status,
        BookingStatus = @booking_status_report;
    
    INSERT INTO LogTable (typeOperation, procedure_name, tableOperation, message) 
    VALUES ('end', @procedure_name, NULL, 'Procedure finished. Host: ' + @host_status + '; Property: ' + @property_status + '; Guest: ' + @guest_status + '; Booking: ' + @booking_status_report);

END
GO

PRINT 'Stored Procedure RegisterUsersPropertyAndBooking_Recoverable created successfully.'
GO



-- 1. Test the CreateBookingAndAddReview procedure
-- Call that will not fail
EXEC CreateBookingAndAddReview
    @property_id = 1,       -- Replace with an actual property_id
    @guest_id = 2,          -- Replace with an actual guest_id
    @start_date = '2025-12-01',
    @end_date = '2025-12-05',
    @booking_status = 'confirmed',
    @review_rating = 5,
    @review_comment = 'Absolutely fantastic stay! The property was amazing and the host was very welcoming.';

-- To see the created booking and review:
SELECT * FROM Bookings WHERE guest_id = 2 AND property_id = 1;
SELECT * FROM Reviews WHERE guest_id = 2 AND property_id = 1;


-- Call that will fail (invalid booking dates)
EXEC CreateBookingAndAddReview
    @property_id = 1,
    @guest_id = 2,
    @start_date = '2022-11-01', -- Start date in the past (relative to GETDATE() if it's past Nov 1, 2025)
    @end_date = '2022-11-05',
    @review_rating = 4,
    @review_comment = 'Good stay.';
-- This should raise an error due to invalid booking dates.


-- 2. Test the RegisterUsersPropertyAndBooking_Recoverable procedure
-- Call that will not fail
EXEC RegisterUsersPropertyAndBooking_Recoverable
    -- Host details
    @host_first_name = 'John',
    @host_last_name = 'Doe',
    @host_email = 'john.doe.host@example.com',
    @host_password = 'SecurePassword123!',
    @host_phone = '555-0101',
    -- Property details
    @prop_title = 'Cozy Downtown Apartment',
    @prop_description = 'A charming apartment in the heart of the city, perfect for weekend getaways.',
    @prop_address = '456 Urban Ln',
    @prop_city = 'Metroville',
    @prop_state = 'Anystate',
    @prop_country = 'USA',
    @prop_zipcode = '90210',
    @prop_property_type = 'apartment',
    @prop_price_per_night = 120.50,
    -- Guest details
    @guest_first_name = 'Alice',
    @guest_last_name = 'Wonder',
    @guest_email = 'alice.guest@example.com',
    @guest_password = 'AnotherSecurePwd987!',
    @guest_phone = '555-0202',
    -- Booking details
    @book_start_date = '2025-01-10',
    @book_end_date = '2025-01-15',
    @book_status = 'pending';

-- This will output the status of each creation step (Host, Property, Guest, Booking).


-- Call that will fail (invalid email for guest)
EXEC RegisterUsersPropertyAndBooking_Recoverable
    -- Host details (assuming this host already exists or is valid)
    @host_first_name = 'Jane',
    @host_last_name = 'Smith',
    @host_email = 'jane.smith.host@example.com',
    @host_password = 'PasswordJane!',
    -- Property details
    @prop_title = 'Seaside Cottage',
    @prop_description = 'Relaxing cottage by the sea.',
    @prop_address = '789 Ocean Ave',
    @prop_city = 'Beachtown',
    @prop_country = 'USA',
    @prop_property_type = 'house',
    @prop_price_per_night = 200.00,
    -- Guest details (invalid email)
    @guest_first_name = 'Bob',
    @guest_last_name = 'Builder',
    @guest_email = 'bob.builder.invalid', -- Invalid email format
    @guest_password = 'BobCanFixIt123',
    -- Booking details
    @book_start_date = '2025-02-20',
    @book_end_date = '2025-02-25';

-- This should show success for Host and Property (if Jane Smith is new or valid),
-- then failure for Guest, and Booking creation will be skipped.


-- 3. Check the LogTable for all operations
SELECT * FROM LogTable ORDER BY log_time DESC;

-- For a specific procedure:
SELECT * FROM LogTable WHERE procedure_name = 'CreateBookingAndAddReview' ORDER BY log_time DESC;
SELECT * FROM LogTable WHERE procedure_name = 'RegisterUsersPropertyAndBooking_Recoverable' ORDER BY log_time DESC;