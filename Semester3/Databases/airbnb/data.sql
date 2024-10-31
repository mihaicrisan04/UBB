INSERT INTO Users (first_name, last_name, email, password, phone, user_type)
VALUES
('Alice', 'Johnson', 'alice.johnson@example.com', 'password123', '123-456-7890', 'host'),
('Bob', 'Smith', 'bob.smith@example.com', 'password456', '234-567-8901', 'guest'),
('Charlie', 'Brown', 'charlie.brown@example.com', 'password789', '345-678-9012', 'guest');

INSERT INTO Properties (host_id, title, description, address, city, state, country, zipcode, property_type, price_per_night)
VALUES
(1, 'Seaside Villa', 'A beautiful villa by the sea', '123 Ocean Drive', 'Miami', 'FL', 'USA', '33101', 'villa', 300.00),
(1, 'Mountain Cabin', 'A cozy cabin in the mountains', '456 Mountain Road', 'Denver', 'CO', 'USA', '80202', 'house', 150.00),
(2, 'City Apartment', 'A modern apartment in the city', '789 City Street', 'New York', 'NY', 'USA', '10001', 'apartment', 200.00),
(2, 'Country Cottage', 'A charming cottage in the countryside', '012 Country Lane', 'Austin', 'TX', 'USA', '73301', 'house', 125.00),
(1, 'Beach House', 'A lovely beach house', '321 Beach Boulevard', 'Los Angeles', 'CA', 'USA', '90001', 'house', 250.00),
(2, 'Lake Cabin', 'A rustic cabin by the lake', '654 Lake Lane', 'Chicago', 'IL', 'USA', '60007', 'house', 100.00);

INSERT INTO Bookings (property_id, guest_id, start_date, end_date, total_price, status)
VALUES
(1, 2, '2023-12-01', '2023-12-10', 3000.00, 'confirmed'),
(2, 2, '2023-12-15', '2023-12-20', 750.00, 'pending'),
(3, 2, '2023-12-25', '2023-12-30', 1250.00, 'confirmed'),
(4, 3, '2023-12-05', '2023-12-15', 1000.00, 'cancelled'),
(5, 3, '2023-12-20', '2023-12-25', 500.00, 'confirmed');

INSERT INTO Reviews (property_id, guest_id, rating, comment)
VALUES
(1, 2, 5, 'Amazing place! Highly recommend.'),
(2, 2, 4, 'Great cabin, but a bit cold.'),
(4, 2, 5, 'Perfect apartment for a city getaway.'),
(4, 3, 3, 'Nice cottage, but too far from town.'),
(5, 3, 4, 'Beautiful beach house with stunning views.');

INSERT INTO Amenities (name)
VALUES
('Wifi'),
('Parking'),
('Kitchen'),
('Pool'),
('Gym'),
('Hot Tub'),
('Fireplace'),
('Balcony'),
('Garden'),
('BBQ'),
('Beach Access'),
('Lake Access'),
('Mountain View'),
('City View'),
('Pet Friendly'),
('Family Friendly'),
('Wheelchair Accessible'),
('Smoking Allowed'),
('Events Allowed'),
('Long Term Stays Allowed');

INSERT INTO Property_Amenities (property_id, amenity_id)
VALUES
(1, 1),
(1, 2),
(1, 3),
(1, 4),
(1, 5),
(1, 6),
(1, 7),
(1, 8),
(1, 9),
(1, 10),
(2, 1),
(2, 2),
(2, 3),
(2, 4),
(2, 7),
(2, 8),
(2, 9),
(2, 10),
(4, 1),
(4, 4),
(4, 5),
(4, 6),
(4, 9),
(4, 10),
(5, 1),
(5, 2),
(5, 5),
(5, 6),
(5, 7),
(5, 8),
(5, 10),
(6, 1),
(6, 2),
(6, 5),
(6, 6),
(6, 7),
(6, 8);

INSERT INTO Favorites (guest_id, property_id)
VALUES
(1, 1),
(1, 2),
(1, 4),
(1, 6),
(1, 7),
(2, 1),
(2, 4),
(2, 5),
(2, 6),
(3, 1),
(3, 2),
(3, 5),
(3, 7);

INSERT INTO Payments (booking_id, amount, payment_method, status, transaction_date)
VALUES
-- Bookings ids: 3, 4, 6, 7, 8
(3, 3000.00, 'credit_card', 'completed', '2023-11-30 10:00:00'),
(4, 750.00, 'paypal', 'completed', '2023-12-01 12:00:00'),
(6, 3000.00, 'credit_card', 'completed', '2023-12-02 14:00:00'),
(7, 750.00, 'credit_card', 'completed', '2023-12-03 16:00:00'),
(8, 500.00, 'credit_card', 'completed', '2023-12-04 18:00:00');

INSERT INTO Property_Photos (property_id, photo_url, uploaded_at)
VALUES
(1, 'https://via.placeholder.com/800x600.png?text=Seaside+Villa', '2023-11-01 10:00:00'),
(1, 'https://via.placeholder.com/800x600.png?text=Seaside+Villa', '2023-11-02 12:00:00'),
(1, 'https://via.placeholder.com/800x600.png?text=Seaside+Villa', '2023-11-03 14:00:00'),
(2, 'https://via.placeholder.com/800x600.png?text=Mountain+Cabin', '2023-11-04 16:00:00'),
(2, 'https://via.placeholder.com/800x600.png?text=Mountain+Cabin', '2023-11-05 18:00:00'),
(2, 'https://via.placeholder.com/800x600.png?text=Mountain+Cabin', '2023-11-06 20:00:00'),
(4, 'https://via.placeholder.com/800x600.png?text=Country+Cottage', '2023-11-10 16:00:00'),
(4, 'https://via.placeholder.com/800x600.png?text=Country+Cottage', '2023-11-11 18:00:00'),
(4, 'https://via.placeholder.com/800x600.png?text=Country+Cottage', '2023-11-12 20:00:00'),
(5, 'https://via.placeholder.com/800x600.png?text=Beach+House', '2023-11-13 10:00:00'),
(5, 'https://via.placeholder.com/800x600.png?text=Beach+House', '2023-11-14 12:00:00'),
(5, 'https://via.placeholder.com/800x600.png?text=Beach+House', '2023-11-15 14:00:00'),
(6, 'https://via.placeholder.com/800x600.png?text=Lake+Cabin', '2023-11-16 16:00:00'),
(6, 'https://via.placeholder.com/800x600.png?text=Lake+Cabin', '2023-11-17 18:00:00'),
(6, 'https://via.placeholder.com/800x600.png?text=Lake+Cabin', '2023-11-18 20:00:00');