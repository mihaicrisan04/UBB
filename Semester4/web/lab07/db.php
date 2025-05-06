<?php
require_once 'config.php';

function getDBConnection() {
    try {
        $conn = new mysqli(DB_HOST, DB_USER, DB_PASS, DB_NAME);
        
        if ($conn->connect_error) {
            throw new Exception("Connection failed: " . $conn->connect_error);
        }
        
        // Set charset to utf8mb4
        if (!$conn->set_charset("utf8mb4")) {
            throw new Exception("Error setting charset: " . $conn->error);
        }
        
        return $conn;
    } catch (Exception $e) {
        die("Database connection failed: " . $e->getMessage());
    }
}

// Function to prevent SQL injection
function sanitizeInput($input) {
    $conn = getDBConnection();
    return $conn->real_escape_string(trim($input));
}

// Function to check if a table exists
function tableExists($tableName) {
    $conn = getDBConnection();
    $result = $conn->query("SHOW TABLES LIKE '$tableName'");
    $exists = $result->num_rows > 0;
    $conn->close();
    return $exists;
}
?> 