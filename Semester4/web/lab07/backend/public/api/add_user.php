<?php
header('Content-Type: application/json');
header('Access-Control-Allow-Origin: *'); 
header('Access-Control-Allow-Methods: POST, OPTIONS');
header('Access-Control-Allow-Headers: Content-Type, Authorization');

if ($_SERVER['REQUEST_METHOD'] == 'OPTIONS') {
    exit(0);
}

require_once __DIR__ . '/../../config.php';
require_once __DIR__ . '/../../db.php';

$response = ['status' => 'error', 'message' => 'Invalid request'];
$conn = null;

if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    // Read JSON data from the request body
    $input = json_decode(file_get_contents('php://input'), true);

    // Basic validation (can be expanded)
    if (empty($input['name']) || empty($input['username']) || empty($input['password']) || empty($input['email']) || empty($input['role']) || empty($input['gender']) || !isset($input['age'])) {
        http_response_code(400); // Bad Request
        $response['message'] = 'Missing required fields.';
    } else {
        // Sanitize input
        $name = sanitizeInput($input['name']);
        $username = sanitizeInput($input['username']);
        $password = password_hash($input['password'], PASSWORD_DEFAULT); // Hash password
        $age = filter_var($input['age'], FILTER_VALIDATE_INT, ['options' => ['min_range' => 0]]);
        $role = sanitizeInput($input['role']);
        $gender = sanitizeInput($input['gender']);
        $email = filter_var($input['email'], FILTER_VALIDATE_EMAIL) ? sanitizeInput($input['email']) : null;
        $webpage = isset($input['webpage']) && filter_var($input['webpage'], FILTER_VALIDATE_URL) ? sanitizeInput($input['webpage']) : null;
        $profile = isset($input['profile']) ? sanitizeInput($input['profile']) : '';
        
        if ($age === false || $email === null) {
             http_response_code(400);
             $response['message'] = 'Invalid age or email format.';
        } else {
            try {
                $conn = getDBConnection();

                // Escape inputs right before query
                $escaped_username = escapeSQL($conn, $username);
                $escaped_email = escapeSQL($conn, $email);

                // Check if username or email already exists
                $check_sql = "SELECT id FROM users WHERE username = ? OR email = ?";
                $check_stmt = $conn->prepare($check_sql);
                if (!$check_stmt) throw new Exception("Prepare failed (check): " . $conn->error);
                $check_stmt->bind_param("ss", $escaped_username, $escaped_email);
                $check_stmt->execute();
                $check_result = $check_stmt->get_result();
                
                if ($check_result->num_rows > 0) {
                    http_response_code(409); // Conflict
                    $response['message'] = 'Username or email already exists!';
                } else {
                    $sql = "INSERT INTO users (name, username, password, age, role, gender, email, webpage, profile) 
                            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";
                    $stmt = $conn->prepare($sql);
                     if (!$stmt) throw new Exception("Prepare failed (insert): " . $conn->error);

                    // Escape other necessary fields
                    $escaped_name = escapeSQL($conn, $name);
                    $escaped_role = escapeSQL($conn, $role);
                    $escaped_gender = escapeSQL($conn, $gender);
                    $escaped_webpage = $webpage ? escapeSQL($conn, $webpage) : null;
                    $escaped_profile = escapeSQL($conn, $profile);

                    $stmt->bind_param("sssisssss", $escaped_name, $escaped_username, $password, $age, $escaped_role, $escaped_gender, $escaped_email, $escaped_webpage, $escaped_profile);
                    
                    if ($stmt->execute()) {
                        $response['status'] = 'success';
                        $response['message'] = 'User added successfully!';
                        $response['userId'] = $conn->insert_id; // Return the ID of the new user
                    } else {
                        throw new Exception("Execute failed: " . $stmt->error);
                    }
                    $stmt->close();
                }
                
                $check_stmt->close();
                $conn->close();

            } catch (Exception $e) {
                http_response_code(500);
                $response['message'] = "Error adding user: " . $e->getMessage();
                if ($conn) $conn->close();
            }
        }
    }
} else {
    http_response_code(405); // Method Not Allowed
    $response['message'] = 'POST method required.';
}

echo json_encode($response);
?> 