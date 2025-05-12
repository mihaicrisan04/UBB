<?php
header('Content-Type: application/json');
header('Access-Control-Allow-Origin: *'); // Allow requests from any origin (adjust for production)
header('Access-Control-Allow-Methods: GET, POST, OPTIONS'); 
header('Access-Control-Allow-Headers: Content-Type, Authorization');

// Handle OPTIONS request (preflight)
if ($_SERVER['REQUEST_METHOD'] == 'OPTIONS') {
    exit(0);
}

require_once __DIR__ . '/../../config.php';
require_once __DIR__ . '/../../db.php';

$response = ['status' => 'success', 'messages' => []];
$conn = null;

try {
    $conn = getDBConnection();

    // Create users table - Added gender column based on setup.sql
    $sql = "CREATE TABLE IF NOT EXISTS users (
        id INT AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(100) NOT NULL,
        username VARCHAR(50) NOT NULL UNIQUE,
        password VARCHAR(255) NOT NULL,
        age INT,
        role VARCHAR(50) NOT NULL,
        gender VARCHAR(10), 
        profile TEXT,
        email VARCHAR(100) NOT NULL UNIQUE,
        webpage VARCHAR(255),
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
    )";

    if ($conn->query($sql) === TRUE) {
        $response['messages'][] = "Table 'users' checked/created successfully.";
    } else {
        throw new Exception("Error creating table: " . $conn->error);
    }

    // Insert sample data (only if table was newly created or empty, adjusted logic)
    $sample_users = [
        [
            'name' => 'Admin User',
            'username' => 'admin',
            'password' => password_hash('admin123', PASSWORD_DEFAULT),
            'age' => 30,
            'role' => 'admin',
            'gender' => 'other',
            'email' => 'admin@example.com',
            'webpage' => 'https://example.com',
            'profile' => 'System administrator'
        ],
        [
            'name' => 'Regular User',
            'username' => 'user',
            'password' => password_hash('user123', PASSWORD_DEFAULT),
            'age' => 25,
            'role' => 'user',
            'gender' => 'male',
            'email' => 'user@example.com',
            'webpage' => 'https://example.com/user',
            'profile' => 'Regular user account'
        ]
    ];

    $insert_sql = "INSERT INTO users (name, username, password, age, role, gender, email, webpage, profile) 
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";
    $insert_stmt = $conn->prepare($insert_sql);

    if (!$insert_stmt) {
        throw new Exception("Error preparing insert statement: " . $conn->error);
    }

    $check_sql = "SELECT id FROM users WHERE username = ? OR email = ?";
    $check_stmt = $conn->prepare($check_sql);
     if (!$check_stmt) {
        throw new Exception("Error preparing check statement: " . $conn->error);
    }


    foreach ($sample_users as $user) {
        // Check if user already exists
        $check_stmt->bind_param("ss", $user['username'], $user['email']);
        $check_stmt->execute();
        $check_result = $check_stmt->get_result();

        if ($check_result->num_rows == 0) {
            $insert_stmt->bind_param("sssisssss", 
                $user['name'], 
                $user['username'], 
                $user['password'], 
                $user['age'], 
                $user['role'], 
                $user['gender'], // Added gender
                $user['email'], 
                $user['webpage'], 
                $user['profile']
            );
            
            if ($insert_stmt->execute()) {
                $response['messages'][] = "Sample user '{$user['username']}' created successfully.";
            } else {
                 $response['messages'][] = "Warning: Error creating sample user '{$user['username']}': " . $insert_stmt->error;
            }
        } else {
            $response['messages'][] = "Sample user '{$user['username']}' already exists.";
        }
        $check_result->free(); // Free result set
    }

    $insert_stmt->close();
    $check_stmt->close();
    $conn->close();
    $response['messages'][] = "Setup completed.";

} catch (Exception $e) {
    http_response_code(500); // Internal Server Error
    $response['status'] = 'error';
    $response['message'] = "Setup failed: " . $e->getMessage();
     if ($conn) $conn->close(); // Ensure connection is closed on error
}

echo json_encode($response);
?> 