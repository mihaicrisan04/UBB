-- Image Voting Application Database Setup
-- Execute this script in MySQL Workbench or MySQL command line

-- Create the database if it doesn't exist
CREATE DATABASE IF NOT EXISTS image_voting_app;

-- Use the database
USE image_voting_app;

-- Create users table
CREATE TABLE IF NOT EXISTS users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    password VARCHAR(255) NOT NULL,
    email VARCHAR(100),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create images table
CREATE TABLE IF NOT EXISTS images (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    filename VARCHAR(255) NOT NULL,
    original_filename VARCHAR(255) NOT NULL,
    title VARCHAR(100),
    description TEXT,
    file_path VARCHAR(500) NOT NULL,
    upload_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    total_votes INT DEFAULT 0,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    INDEX idx_user_id (user_id),
    INDEX idx_total_votes (total_votes)
);

-- Create votes table
CREATE TABLE IF NOT EXISTS votes (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT NOT NULL,
    image_id INT NOT NULL,
    vote_value INT NOT NULL CHECK (vote_value > 0),
    vote_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (image_id) REFERENCES images(id) ON DELETE CASCADE,
    UNIQUE KEY unique_user_image_vote (user_id, image_id),
    INDEX idx_image_id (image_id),
    INDEX idx_user_id (user_id)
);

-- Create trigger to update total_votes in images table when votes are added/updated/deleted
DELIMITER //

CREATE TRIGGER update_image_votes_after_insert
AFTER INSERT ON votes
FOR EACH ROW
BEGIN
    UPDATE images 
    SET total_votes = (
        SELECT COALESCE(SUM(vote_value), 0) 
        FROM votes 
        WHERE image_id = NEW.image_id
    )
    WHERE id = NEW.image_id;
END//

CREATE TRIGGER update_image_votes_after_update
AFTER UPDATE ON votes
FOR EACH ROW
BEGIN
    UPDATE images 
    SET total_votes = (
        SELECT COALESCE(SUM(vote_value), 0) 
        FROM votes 
        WHERE image_id = NEW.image_id
    )
    WHERE id = NEW.image_id;
END//

CREATE TRIGGER update_image_votes_after_delete
AFTER DELETE ON votes
FOR EACH ROW
BEGIN
    UPDATE images 
    SET total_votes = (
        SELECT COALESCE(SUM(vote_value), 0) 
        FROM votes 
        WHERE image_id = OLD.image_id
    )
    WHERE id = OLD.image_id;
END//

DELIMITER ;

-- Insert test users
INSERT INTO users (username, password, email) VALUES 
('admin', 'admin123', 'admin@example.com'),
('testuser', 'test123', 'test@example.com'),
('photographer', 'photo123', 'photo@example.com');

-- Display created tables
SHOW TABLES;

-- Display table structures
DESCRIBE users;
DESCRIBE images;
DESCRIBE votes; 