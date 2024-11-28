


-- example of a stored procedure
CREATE PROCEDURE sp_getPropertyDetails
    @property_id INT
AS
BEGIN
    SELECT * FROM Properties WHERE property_id = @property_id;
END;
GO
DROP PROCEDURE sp_getPropertyDetails;

EXEC sp_getPropertyDetails 1;



-- a) change the type of a column
-- change description type from text to varchar from the Properties table
CREATE PROCEDURE sp_changeDescriptionTypeToVarchar
AS
BEGIN
    ALTER TABLE Properties ALTER COLUMN description VARCHAR(MAX);
END;
GO

-- change description type from varchar to text from the Properties table
CREATE PROCEDURE sp_changeDescriptionTypeToText
AS
BEGIN
    ALTER TABLE Properties ALTER COLUMN description TEXT;
END;
GO

DROP PROCEDURE sp_changeDescriptionTypeToVarchar;
DROP PROCEDURE sp_changeDescriptionTypeToText;

EXEC sp_changeDescriptionTypeToVarchar;
EXEC sp_changeDescriptionTypeToText;



-- b) add/remove a column
-- add a stars column to the Properties table
CREATE PROCEDURE sp_addPropertyStarsColumn
AS
BEGIN
    ALTER TABLE Properties ADD stars DECIMAL(2, 1);
END;
GO

-- remove the stars column from the Properties table
CREATE PROCEDURE sp_removePropertyStarsColumn
AS
BEGIN
    ALTER TABLE Properties DROP COLUMN stars;
END;
GO

DROP PROCEDURE sp_addPropertyStarsColumn;
DROP PROCEDURE sp_removePropertyStarsColumn;

EXEC sp_addPropertyStarsColumn;
EXEC sp_removePropertyStarsColumn;



-- c) add/remove a foreign key constraint
-- add a default constraint for the total_price column in the Bookings table
CREATE PROCEDURE sp_addDefaultConstraintForPriceInBookings
AS
BEGIN
    ALTER TABLE Bookings ADD CONSTRAINT DF_Price DEFAULT 0 FOR total_price;
END;
GO

-- remove the default constraint for the total_price column in the Bookings table
CREATE PROCEDURE sp_removeDefaultConstraintForPriceInBookings
AS
BEGIN
    ALTER TABLE Bookings DROP CONSTRAINT DF_Price;
END;
GO

DROP PROCEDURE sp_addDefaultConstraintForPriceInBookings;
DROP PROCEDURE sp_removeDefaultConstraintForPriceInBookings;

EXEC sp_addDefaultConstraintForPriceInBookings;
EXEC sp_removeDefaultConstraintForPriceInBookings;





-- d) add/remove a primary key constraint
-- add a primary key constraint to the messages table
CREATE PROCEDURE sp_addPrimaryKeyToMessages
AS
BEGIN
    ALTER TABLE Messages ADD CONSTRAINT PK_Messages PRIMARY KEY (message_id);
END;
GO

-- remove the primary key from the Messages table
CREATE PROCEDURE sp_removePrimaryKeyFromMessages
AS
BEGIN
    ALTER TABLE Messages DROP CONSTRAINT PK_Messages;
END;
GO

DROP PROCEDURE sp_addPrimaryKeyToMessages;
DROP PROCEDURE sp_removePrimaryKeyFromMessages;

EXEC sp_addPrimaryKeyToMessages;
EXEC sp_removePrimaryKeyFromMessages;




-- e) add/remove a candidate key constraint
-- add a candidate key constraint to the Users table
CREATE PROCEDURE sp_addCandidateKeyToUsers
AS
BEGIN
    ALTER TABLE Users ADD CONSTRAINT UQ_Email UNIQUE (email);
END;
GO

-- remove the candidate key constraint from the Users table
CREATE PROCEDURE sp_removeCandidateKeyFromUsers
AS
BEGIN
    ALTER TABLE Users DROP CONSTRAINT UQ_Email;
END;
GO

DROP PROCEDURE sp_addCandidateKeyToUsers;
DROP PROCEDURE sp_removeCandidateKeyFromUsers;

EXEC sp_addCandidateKeyToUsers;
EXEC sp_removeCandidateKeyFromUsers;



-- f) add/remove a foreign key constraint
CREATE PROCEDURE sp_addForeignKeyToBookings
AS
BEGIN
    ALTER TABLE Bookings ADD CONSTRAINT FK_Bookings_Properties FOREIGN KEY (property_id) REFERENCES Properties(property_id);
END;
GO

CREATE PROCEDURE sp_removeForeignKeyFromBookings
AS
BEGIN
    ALTER TABLE Bookings DROP CONSTRAINT FK_Bookings_Properties;
END;
GO

DROP PROCEDURE sp_addForeignKeyToBookings;
DROP PROCEDURE sp_removeForeignKeyFromBookings;

EXEC sp_addForeignKeyToBookings;
EXEC sp_removeForeignKeyFromBookings;



-- g) create/drop a table
CREATE PROCEDURE sp_createTable
AS
BEGIN
    CREATE TABLE TestTable (
        id INT PRIMARY KEY,
        name VARCHAR(100)
    );
END;
GO

CREATE PROCEDURE sp_dropTable
AS
BEGIN
    DROP TABLE TestTable;
END;
GO

DROP PROCEDURE sp_createTable;
DROP PROCEDURE sp_dropTable;

EXEC sp_createTable;
EXEC sp_dropTable;



-- versioning schema
CREATE PROCEDURE sp_switchVersion
    @version INT
AS
BEGIN
    DECLARE @currentVersion INT;
    SELECT @currentVersion = version FROM SchemaVersion;

    IF @version > @currentVersion
    BEGIN
        -- Upgrade logic
        IF @currentVersion = 1 AND @version >= 2
        BEGIN
            EXEC sp_changeDescriptionTypeToVarchar;
            EXEC sp_removePropertyStarsColumn;
            EXEC sp_removeDefaultConstraintForPriceInBookings;
            EXEC sp_addPrimaryKeyToMessages;
            EXEC sp_addCandidateKeyToUsers;
            EXEC sp_addForeignKeyToBookings;
            EXEC sp_dropTable;
        END
    END
    ELSE IF @version < @currentVersion
    BEGIN
        -- Downgrade logic
        IF @currentVersion = 2 AND @version <= 1
        BEGIN
            EXEC sp_changeDescriptionTypeToText;
            EXEC sp_addPropertyStarsColumn;
            EXEC sp_addDefaultConstraintForPriceInBookings;
            EXEC sp_removePrimaryKeyFromMessages;
            EXEC sp_removeCandidateKeyFromUsers;
            EXEC sp_removeForeignKeyFromBookings;
            EXEC sp_createTable;
        END
    END

    -- Update the version
    UPDATE SchemaVersion SET version = @version;
END;
GO

DROP PROCEDURE sp_switchVersion;

EXEC sp_switchVersion 2;
EXEC sp_switchVersion 1;
