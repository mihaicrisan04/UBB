DELETE FROM Users;
INSERT INTO Users (first_name, last_name, email, password, phone, user_type)
VALUES
('Alice', 'Johnson', 'alice.johnson@example.com', 'password123', '123-456-7890', 'host'),
('Bob', 'Smith', 'bob.smith@example.com', 'password456', '234-567-8901', 'guest');

DELETE FROM Properties;
INSERT INTO Properties (host_id, title, description, address, city, state, country, zipcode, property_type, price_per_night)
VALUES
(1, 'Seaside Villa', 'A beautiful villa by the sea', '123 Ocean Drive', 'Miami', 'FL', 'USA', '33101', 'villa', 300.00),
(1, 'Mountain Cabin', 'A cozy cabin in the mountains', '456 Mountain Road', 'Denver', 'CO', 'USA', '80202', 'house', 150.00);

DELETE FROM Bookings;
INSERT INTO Bookings (property_id, guest_id, start_date, end_date, total_price, status)
VALUES
(1, 2, '2023-12-01', '2023-12-10', 3000.00, 'confirmed'),
(2, 2, '2023-12-15', '2023-12-20', 750.00, 'pending');

DELETE FROM Reviews;
INSERT INTO Reviews (property_id, guest_id, rating, comment)
VALUES
(1, 2, 5, 'Amazing place! Highly recommend.'),
(2, 2, 4, 'Great cabin, but a bit cold.');

-- fail because there is no user with user_id = 999
INSERT INTO Properties (host_id, title, description, address, city, state, country, zipcode, property_type, price_per_night)
VALUES
(999, 'Nonexistent Host Property', 'This should fail', '789 Nowhere Lane', 'Nowhere', 'NA', 'NA', '00000', 'apartment', 100.00);



UPDATE Users
SET phone = '987-654-3210'
WHERE email = 'alice.johnson@example.com';

UPDATE Properties
SET price_per_night = 350.00
WHERE property_type = 'villa' AND city = 'Miami';

UPDATE Bookings
SET status = 'cancelled'
WHERE total_price > 1000.00 OR status = 'pending';

UPDATE Users
SET phone = '000-000-0000'
WHERE phone IS NULL;

UPDATE Bookings
SET status = 'confirmed'
WHERE total_price IN (750.00, 3000.00);

UPDATE Properties
SET price_per_night = price_per_night * 1.1
WHERE price_per_night BETWEEN 100.00 AND 200.00;

UPDATE Properties
SET description = 'Updated description for villa'
WHERE title LIKE '%Villa%';



DELETE FROM Reviews
WHERE rating < 4;

DELETE FROM Bookings
WHERE end_date BETWEEN '2023-12-01' AND '2023-12-31' OR status = 'cancelled';



-- a)
SELECT first_name, last_name, email
FROM Users
WHERE user_type = 'host' OR email LIKE '%example.com'
UNION
SELECT first_name, last_name, email
FROM Users
WHERE user_type = 'guest';

SELECT property_id, title, city
FROM Properties
WHERE property_type = 'villa' OR city = 'Miami'
UNION ALL
SELECT property_id, title, city
FROM Properties
WHERE price_per_night > 200;


-- b)
SELECT user_id, first_name, last_name
FROM Users
WHERE user_id IN (SELECT host_id FROM Properties)
INTERSECT
SELECT user_id, first_name, last_name
FROM Users
WHERE user_type = 'host';

SELECT property_id, title
FROM Properties
WHERE property_id IN (SELECT property_id FROM Bookings WHERE status = 'confirmed')
INTERSECT
SELECT property_id, title
FROM Properties
WHERE price_per_night > 200;


-- c)
SELECT user_id, first_name, last_name
FROM Users
WHERE user_type = 'guest'
EXCEPT
SELECT user_id, first_name, last_name
FROM Users
WHERE user_id NOT IN (SELECT guest_id FROM Bookings);

SELECT property_id, title
FROM Properties
WHERE property_type = 'house'
EXCEPT
SELECT property_id, title
FROM Properties
WHERE property_id NOT IN (SELECT property_id FROM Bookings WHERE status = 'pending');

-- more deletes
-- more data