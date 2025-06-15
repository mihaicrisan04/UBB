-- Create the database
CREATE DATABASE IF NOT EXISTS ReservationDB;
USE ReservationDB;

-- Drop existing tables to start fresh
DROP TABLE IF EXISTS Reservations;
DROP TABLE IF EXISTS Flights;
DROP TABLE IF EXISTS Hotels;

-- Create the Flights table
CREATE TABLE Flights (
    flightID INT AUTO_INCREMENT PRIMARY KEY,
    date DATE NOT NULL,
    destinationCity VARCHAR(255) NOT NULL,
    availableSeats INT NOT NULL
);

-- Create the Hotels table
CREATE TABLE Hotels (
    hotelID INT AUTO_INCREMENT PRIMARY KEY,
    hotelName VARCHAR(255) NOT NULL,
    date DATE NOT NULL,
    city VARCHAR(255) NOT NULL,
    availableRooms INT NOT NULL
);

-- Create the Reservations table
CREATE TABLE Reservations (
    id INT AUTO_INCREMENT PRIMARY KEY,
    person VARCHAR(255) NOT NULL,
    date DATE NOT NULL,
    type VARCHAR(50) NOT NULL, -- "Flight" or "Hotel"
    idReservedResource INT NOT NULL
);

-- Insert seed data
INSERT INTO Flights (date, destinationCity, availableSeats) VALUES
('2025-08-10', 'Paris', 50),
('2025-08-10', 'Tokyo', 25),
('2025-08-12', 'Paris', 10);

INSERT INTO Hotels (hotelName, date, city, availableRooms) VALUES
('Grand Parisian Hotel', '2025-08-10', 'Paris', 15),
('Hotel Lumière', '2025-08-10', 'Paris', 3),
('Tokyo Palace', '2025-08-10', 'Tokyo', 20);

-- Add a past reservation to test the cancellation logic
INSERT INTO Reservations (person, date, type, idReservedResource) VALUES
('Mihai', '2024-07-20', 'Flight', 1);