<?php
require_once 'config.php';
require_once 'db.php';

$conn = getDBConnection();

// Create users table
$sql = "CREATE TABLE IF NOT EXISTS users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    username VARCHAR(50) NOT NULL UNIQUE,
    password VARCHAR(255) NOT NULL,
    age INT,
    role VARCHAR(50) NOT NULL,
    profile TEXT,
    email VARCHAR(100) NOT NULL UNIQUE,
    webpage VARCHAR(255),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
)";

if ($conn->query($sql) === TRUE) {
    echo "Table 'users' created successfully<br>";
} else {
    echo "Error creating table: " . $conn->error . "<br>";
}

// Insert some sample data
$sample_users = [
    [
        'name' => 'Admin User',
        'username' => 'admin',
        'password' => password_hash('admin123', PASSWORD_DEFAULT),
        'age' => 30,
        'role' => 'admin',
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
        'email' => 'user@example.com',
        'webpage' => 'https://example.com/user',
        'profile' => 'Regular user account'
    ]
];

foreach ($sample_users as $user) {
    // Check if user already exists
    $check_sql = "SELECT id FROM users WHERE username = ? OR email = ?";
    $check_stmt = $conn->prepare($check_sql);
    $check_stmt->bind_param("ss", $user['username'], $user['email']);
    $check_stmt->execute();
    $check_result = $check_stmt->get_result();
    
    if ($check_result->num_rows == 0) {
        $insert_sql = "INSERT INTO users (name, username, password, age, role, email, webpage, profile) 
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?)";
        $insert_stmt = $conn->prepare($insert_sql);
        $insert_stmt->bind_param("sssissss", 
            $user['name'], 
            $user['username'], 
            $user['password'], 
            $user['age'], 
            $user['role'], 
            $user['email'], 
            $user['webpage'], 
            $user['profile']
        );
        
        if ($insert_stmt->execute()) {
            echo "Sample user '{$user['username']}' created successfully<br>";
        } else {
            echo "Error creating sample user '{$user['username']}': " . $insert_stmt->error . "<br>";
        }
        
        $insert_stmt->close();
    } else {
        echo "Sample user '{$user['username']}' already exists<br>";
    }
    
    $check_stmt->close();
}

$conn->close();

echo "<br>Setup completed. <a href='index.php'>Go to Home Page</a>";
?> 