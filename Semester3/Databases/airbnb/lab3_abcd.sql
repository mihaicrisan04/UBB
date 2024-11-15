

CREATE PROCEDURE sp_getPropertyDetails
    @property_id INT
AS
BEGIN
    SELECT * FROM Properties WHERE property_id = @property_id;
END;
GO

EXEC sp_getPropertyDetails 1;


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


-- change description type from text to varchar from the Properties table
CREATE PROCEDURE sp_changeDescriptionTypeToVarchar
AS
BEGIN
    ALTER TABLE Properties ALTER COLUMN description VARCHAR(255);
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



-- add a primary key to the Amenities table
CREATE PROCEDURE sp_addPrimaryKeyToAmenities
AS
BEGIN
    ALTER TABLE Amenities ADD amenity_id INT PRIMARY KEY IDENTITY(1,1);
END;


--
CREATE PROCEDURE sp_addDefaultConstraintForPriceInBookings
AS
BEGIN
    ALTER TABLE Bookings ADD CONSTRAINT DF_Price DEFAULT 0 FOR total_price;
END;
GO

DROP PROCEDURE sp_addDefaultConstraintForPriceInBookings;

EXEC sp_addDefaultConstraintForPriceInBookings;

