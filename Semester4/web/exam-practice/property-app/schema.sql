CREATE DATABASE IF NOT EXISTS property_manager_db;
USE property_manager_db;

CREATE TABLE User (
    id INT PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(255) NOT NULL UNIQUE,
    secretQuestion VARCHAR(255) NOT NULL,
    secretAnswer VARCHAR(255) NOT NULL
);

CREATE TABLE Property (
    id INT PRIMARY KEY AUTO_INCREMENT,
    address VARCHAR(255) NOT NULL UNIQUE,
    description TEXT
);

CREATE TABLE UserToProperties (
    id INT PRIMARY KEY AUTO_INCREMENT,
    idUser INT,
    idProperty INT,
    FOREIGN KEY (idUser) REFERENCES User(id),
    FOREIGN KEY (idProperty) REFERENCES Property(id),
    UNIQUE(idUser, idProperty)
);