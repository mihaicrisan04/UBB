

-- d)
SELECT
    Users.first_name,
    Users.last_name,
    Properties.title,
    Bookings.start_date,
    Bookings.end_date
FROM
    Users
INNER JOIN
    Properties ON Users.user_id = Properties.host_id
INNER JOIN
    Bookings ON Properties.property_id = Bookings.property_id
WHERE
    Users.user_type = 'host';

SELECT * from Users
select * from Properties
select * from Bookings


SELECT
    Properties.title,
    Amenities.name
FROM
    Properties
LEFT JOIN
    Property_Amenities ON Properties.property_id = Property_Amenities.property_id
LEFT JOIN
    Amenities ON Property_Amenities.amenity_id = Amenities.amenity_id;


SELECT
    Users.first_name,
    Users.last_name,
    Reviews.rating,
    Reviews.comment
FROM
    Reviews
RIGHT JOIN
    Users ON Reviews.guest_id = Users.user_id;


SELECT
    Users.first_name,
    Users.last_name,
    Messages.message_text,
    Messages.sent_at
FROM
    Users
FULL JOIN
    Messages ON Users.user_id = Messages.sender_id;


-- e)
SELECT
    title,
    city
FROM
    Properties
WHERE
    property_id IN (
        SELECT
            property_id
        FROM
            Bookings
        WHERE
            guest_id IN (
                SELECT
                    user_id
                FROM
                    Users
                WHERE
                    email LIKE '%example.com'
            )
    );

select * from Properties
select * from Bookings
select * from Users

SELECT
    first_name,
    last_name
FROM
    Users
WHERE
    user_id IN (
        SELECT
            guest_id
        FROM
            Bookings
        WHERE
            status = 'confirmed'
    );

-- f)
SELECT
    title,
    city
FROM
    Properties
WHERE
    EXISTS (
        SELECT
            1
        FROM
            Bookings
        WHERE
            Properties.property_id = Bookings.property_id
            AND Bookings.status = 'confirmed'
    );

select * from Bookings
select * from Properties

-- users that left a review with a rating of 4 or higher
SELECT
    first_name,
    last_name
FROM
    Users
WHERE
    EXISTS (
        SELECT
            1
        FROM
            Reviews
        WHERE
            Users.user_id = Reviews.guest_id
            AND Reviews.rating >= 4
    );

-- g)
SELECT
    avg_price_per_night
FROM
    (SELECT
        AVG(price_per_night) AS avg_price_per_night
    FROM
        Properties) AS avg_price;

SELECT
    user_id,
    total_bookings
FROM
    (SELECT
        guest_id AS user_id,
        COUNT(*) AS total_bookings
    FROM
        Bookings
    GROUP BY
        guest_id) AS booking_counts;

-- h)
SELECT
    city,
    COUNT(*) AS property_count
FROM
    Properties
GROUP BY
    city;

-- host with the number of properties larger than the average number of properties per host
SELECT
    host_id,
    COUNT(*) AS property_count
FROM
    Properties
GROUP BY
    host_id
HAVING
    COUNT(*) >= (
        SELECT
            AVG(property_count)
        FROM
            (SELECT
                host_id,
                COUNT(*) AS property_count
            FROM
                Properties
            GROUP BY
                host_id) AS subquery
    );

SELECT
    guest_id,
    SUM(total_price) AS total_spent
FROM
    Bookings
GROUP BY
    guest_id
HAVING
    SUM(total_price) > (
        SELECT
            AVG(total_spent)
        FROM
            (SELECT
                guest_id,
                SUM(total_price) AS total_spent
            FROM
                Bookings
            GROUP BY
                guest_id) AS subquery
    );

SELECT
    property_id,
    SUM(total_price) AS total_revenue
FROM
    Bookings
GROUP BY
    property_id
HAVING
    SUM(total_price) > 1000;