<?php
require_once __DIR__ . '/config.php';

function getDBConnection() {
    try {
        // Use defined constants from config.php
        $conn = new mysqli(DB_HOST, DB_USER, DB_PASS, DB_NAME);

        // Check connection
        if ($conn->connect_error) {
            // Log error instead of dying directly in production
            error_log("Connection failed: " . $conn->connect_error);
            // For API, return a JSON error response or throw an exception
            throw new Exception("Database connection failed."); 
        }

        // Set charset to utf8mb4 for better character support
        if (!$conn->set_charset("utf8mb4")) {
             error_log("Error setting charset: " . $conn->error);
             throw new Exception("Database character set configuration failed.");
        }

        return $conn;
    } catch (Exception $e) {
        // Handle connection error gracefully for an API
        http_response_code(500); // Internal Server Error
        echo json_encode(['status' => 'error', 'message' => $e->getMessage()]);
        exit; // Stop script execution
    }
}

// Basic input sanitization (consider more robust methods for production)
function sanitizeInput($input) {
    // Trim whitespace
    $input = trim($input);
    // Prevent basic XSS if data is ever echoed directly (use ENT_QUOTES for quotes)
    $input = htmlspecialchars($input, ENT_QUOTES, 'UTF-8'); 
    // Note: real_escape_string is applied separately via escapeSQL
    return $input; 
}

// mysqli::real_escape_string needs an active connection.
// This function applies it just before using in a query.
function escapeSQL($conn, $input) {
    return $conn->real_escape_string($input);
}


// Function to check if a table exists
function tableExists($tableName) {
    $conn = null; // Initialize $conn
    try {
        $conn = getDBConnection();
        // Use prepared statement for table name check (though less critical here)
        $stmt = $conn->prepare("SHOW TABLES LIKE ?");
        $escapedTableName = escapeSQL($conn, $tableName); // Escape table name just in case
        $stmt->bind_param("s", $escapedTableName);
        $stmt->execute();
        $result = $stmt->get_result();
        $exists = $result->num_rows > 0;
        $stmt->close();
        $conn->close();
        return $exists;
    } catch (Exception $e) {
         if ($conn) $conn->close();
         // Log the error
         error_log("Error checking table existence for $tableName: " . $e->getMessage());
         // Depending on context, you might want to return false or re-throw
         return false; 
    }
}

?> 