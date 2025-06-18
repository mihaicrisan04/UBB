DROP DATABASE IF EXISTS author_media_db;

CREATE DATABASE author_media_db;
USE author_media_db;

CREATE TABLE Authors (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL UNIQUE,
    documentList VARCHAR(1024), -- Comma-separated list of document IDs
    movieList VARCHAR(1024)    -- Comma-separated list of movie IDs
);

CREATE TABLE Documents (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    contents TEXT
);

CREATE TABLE Movies (
    id INT AUTO_INCREMENT PRIMARY KEY,
    title VARCHAR(255) NOT NULL,
    duration INT -- Duration in minutes
);

INSERT INTO
    Authors (name, documentList, movieList)
VALUES
    ('John Doe', '1,3', '1'),
    ('Jane Smith', '2,3', '1,2');

INSERT INTO
    Documents (name, contents)
VALUES
    ('JSP Basics', 'An introduction to JSP...'),
    ('Advanced SQL', 'A guide to complex queries...'),
    (
        'Collaborative Writing',
        'This document is written by multiple authors.'
    );

INSERT INTO
    Movies (title, duration)
VALUES
    ('The Java Story', 120),
    ('Web Warriors', 95);