

-- g)

-- average price per night
SELECT
    avg_price_per_night
FROM
    (SELECT
        AVG(price_per_night) AS avg_price_per_night
    FROM
        Properties) AS avg_price;


-- total bookings per guest
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

-- number of properties per city
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


-- guest with the total spent larger than the average total spent per guest
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


-- property with the total revenue larger than 1000
SELECT
    property_id,
    SUM(total_price) AS total_revenue
FROM
    Bookings
GROUP BY
    property_id
HAVING
    SUM(total_price) > 1000;


-- i)

-- users who have made bookings with total price larger than 1000
SELECT
    first_name,
    last_name
FROM
    Users
WHERE
    user_id = ANY (SELECT guest_id FROM Bookings WHERE total_price > 500);

-- rewrite with in
SELECT
    first_name,
    last_name
FROM
    Users
WHERE
    user_id IN (SELECT guest_id FROM Bookings WHERE total_price > 1000);


-- properties with a price per night higher than all properties in Austin
SELECT
    title,
    price_per_night
FROM
    Properties
WHERE
    price_per_night > ALL (SELECT price_per_night FROM Properties WHERE city = 'Austin');


-- users who have spent more than 1000 in total bookings
SELECT
    first_name,
    last_name
FROM
    Users
WHERE
    user_id = ANY (SELECT guest_id FROM Bookings GROUP BY guest_id HAVING SUM(total_price) > 1000);

-- properties with a price per night higher than the average price per night in any city
SELECT
    title,
    price_per_night
FROM
    Properties
WHERE
    price_per_night >= ALL (SELECT AVG(price_per_night) FROM Properties GROUP BY city);

-- top 5 properties with the highest price per night in cities where the price per night is higher than the average price per night
SELECT TOP 2
    title,
    price_per_night
FROM
    Properties
WHERE
    price_per_night >= (SELECT AVG(price_per_night) FROM Properties)
ORDER BY
    price_per_night DESC;

-- users who have spent more than 100 in total bookings
SELECT DISTINCT
    first_name,
    last_name
FROM
    Users
WHERE
    user_id IN (SELECT guest_id FROM Bookings GROUP BY guest_id HAVING SUM(total_price) > 100)
ORDER BY
    last_name;
