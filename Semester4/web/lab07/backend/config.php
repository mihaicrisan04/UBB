<?php
// Database configuration
define('DB_HOST', 'localhost');
define('DB_USER', 'root'); 
define('DB_PASS', ''); 
define('DB_NAME', 'lab07_db'); // Suggesting a new DB name for the new project

// Error reporting (adjust for production)
error_reporting(E_ALL);
ini_set('display_errors', 1);

// Session (optional for stateless API, but kept for potential future use)
// If using sessions, configure session handling appropriately for APIs
// session_start(); 

?> 