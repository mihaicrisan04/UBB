<?php
header('Content-Type: text/plain; charset=utf-8'); // Output as plain text for CLI

require_once '../../config.php';
require_once '../../db.php';

$conn = getDBConnection();

if (!$conn) {
    echo "Error: Could not connect to the database.\n";
    exit(1);
}

// Define 30 hardcoded users
// Using simple names for demonstration. Replace with more diverse data if needed.
$users_to_add = [];
for ($i = 1; $i <= 30; $i++) {
    $gender = ($i % 2 == 0) ? 'female' : 'male';
    $role = ($i % 5 == 0) ? 'admin' : 'user'; // Assign admin role every 5 users
    $users_to_add[] = [
        'name' => "User " . ($i + 5), // Start naming from User 6 to avoid conflicts with setup
        'username' => "user" . ($i + 5),
        'email' => "user" . ($i + 5) . "@example.com",
        'password' => "password" . ($i + 5), // Plain password, will be hashed
        'role' => $role,
        'gender' => $gender,
        'age' => null,       // Add age, defaulting to null
        'profile' => null,   // Add profile, defaulting to null
        'webpage' => null    // Add webpage, defaulting to null
    ];
}

// Prepare SQL statement - include new columns
$sql = "INSERT INTO users (name, username, email, password, role, gender, age, profile, webpage) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";
$stmt = $conn->prepare($sql);

if (!$stmt) {
    echo "Error preparing statement: " . $conn->error . "\n";
    $conn->close();
    exit(1);
}

$added_count = 0;
$errors = [];

foreach ($users_to_add as $user) {
    $name = sanitizeInput($user['name']);
    $username = sanitizeInput($user['username']);
    $email = sanitizeInput($user['email']);
    $role = sanitizeInput($user['role']);
    $gender = sanitizeInput($user['gender']);
    $hashed_password = password_hash($user['password'], PASSWORD_DEFAULT); // Hash the password
    $age = $user['age'];
    $profile = $user['profile'];
    $webpage = $user['webpage'];

    $stmt->bind_param("ssssssiss", $name, $username, $email, $hashed_password, $role, $gender, $age, $profile, $webpage);

    if ($stmt->execute()) {
        $added_count++;
    } else {
        // Capture error but continue trying to add others
        $errors[] = "Error adding user {$username}: " . $stmt->error;
    }
}

$stmt->close();
$conn->close();

echo "--- User Addition Report ---\n";
echo "Successfully added {$added_count} users.\n";

if (!empty($errors)) {
    echo "\nEncountered " . count($errors) . " errors:\n";
    foreach ($errors as $error) {
        echo "- " . $error . "\n";
    }
}

?> 