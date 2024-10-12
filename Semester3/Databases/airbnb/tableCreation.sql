

-- 1. Users Table
CREATE TABLE Users (
    user_id INT PRIMARY KEY IDENTITY(1,1),
    first_name VARCHAR(100) NOT NULL,
    last_name VARCHAR(100) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    password VARCHAR(255) NOT NULL,
    phone VARCHAR(20),
    user_type VARCHAR(10) CHECK (user_type IN ('host', 'guest')),
    created_at DATETIME DEFAULT GETDATE()
);

-- 2. Properties Table
CREATE TABLE Properties (
    property_id INT PRIMARY KEY IDENTITY(1,1),
    host_id INT NOT NULL,
    title VARCHAR(255) NOT NULL,
    description TEXT,
    address VARCHAR(255),
    city VARCHAR(100),
    state VARCHAR(100),
    country VARCHAR(100),
    zipcode VARCHAR(20),
    property_type VARCHAR(50) CHECK (property_type IN ('apartment', 'house', 'villa')),
    price_per_night DECIMAL(10, 2) NOT NULL,
    created_at DATETIME DEFAULT GETDATE(),
    FOREIGN KEY (host_id) REFERENCES Users(user_id)
);

-- 3. Bookings Table
CREATE TABLE Bookings (
    booking_id INT PRIMARY KEY IDENTITY(1,1),
    property_id INT NOT NULL,
    guest_id INT NOT NULL,
    start_date DATE NOT NULL,
    end_date DATE NOT NULL,
    total_price DECIMAL(10, 2) NOT NULL,
    status VARCHAR(20) CHECK (status IN ('pending', 'confirmed', 'cancelled')),
    created_at DATETIME DEFAULT GETDATE(),
    FOREIGN KEY (property_id) REFERENCES Properties(property_id),
    FOREIGN KEY (guest_id) REFERENCES Users(user_id)
);

-- 4. Reviews Table
CREATE TABLE Reviews (
    review_id INT PRIMARY KEY IDENTITY(1,1),
    property_id INT NOT NULL,
    guest_id INT NOT NULL,
    rating INT CHECK (rating BETWEEN 1 AND 5),
    comment TEXT,
    created_at DATETIME DEFAULT GETDATE(),
    FOREIGN KEY (property_id) REFERENCES Properties(property_id),
    FOREIGN KEY (guest_id) REFERENCES Users(user_id)
);

-- 5. Payments Table
CREATE TABLE Payments (
    payment_id INT PRIMARY KEY IDENTITY(1,1),
    booking_id INT NOT NULL,
    amount DECIMAL(10, 2) NOT NULL,
    payment_method VARCHAR(20) CHECK (payment_method IN ('credit_card', 'paypal', 'bank_transfer')),
    status VARCHAR(20) CHECK (status IN ('pending', 'completed', 'failed')),
    transaction_date DATETIME DEFAULT GETDATE(),
    FOREIGN KEY (booking_id) REFERENCES Bookings(booking_id)
);

-- 6. Property_Photos Table
CREATE TABLE Property_Photos (
    photo_id INT PRIMARY KEY IDENTITY(1,1),
    property_id INT NOT NULL,
    photo_url VARCHAR(255) NOT NULL,
    uploaded_at DATETIME DEFAULT GETDATE(),
    FOREIGN KEY (property_id) REFERENCES Properties(property_id)
);

-- 7. Amenities Table
CREATE TABLE Amenities (
    amenity_id INT PRIMARY KEY IDENTITY(1,1),
    name VARCHAR(100) UNIQUE NOT NULL
);

-- 8. Property_Amenities Junction Table
CREATE TABLE Property_Amenities (
    property_id INT NOT NULL,
    amenity_id INT NOT NULL,
    FOREIGN KEY (property_id) REFERENCES Properties(property_id),
    FOREIGN KEY (amenity_id) REFERENCES Amenities(amenity_id),
    PRIMARY KEY (property_id, amenity_id)
);

-- 9. Messages Table
CREATE TABLE Messages (
    message_id INT PRIMARY KEY IDENTITY(1,1),
    sender_id INT NOT NULL,
    receiver_id INT NOT NULL,
    message_text TEXT NOT NULL,
    sent_at DATETIME DEFAULT GETDATE(),
    FOREIGN KEY (sender_id) REFERENCES Users(user_id),
    FOREIGN KEY (receiver_id) REFERENCES Users(user_id)
);

-- 10. Favorites Table
CREATE TABLE Favorites (
    favorite_id INT PRIMARY KEY IDENTITY(1,1),
    guest_id INT NOT NULL,
    property_id INT NOT NULL,
    added_at DATETIME DEFAULT GETDATE(),
    FOREIGN KEY (guest_id) REFERENCES Users(user_id),
    FOREIGN KEY (property_id) REFERENCES Properties(property_id)
);

