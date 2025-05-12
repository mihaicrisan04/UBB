<?php
header('Content-Type: application/json');
header('Access-Control-Allow-Origin: *'); 
header('Access-Control-Allow-Methods: PUT, POST, OPTIONS'); // Allow PUT for updates (or POST)
header('Access-Control-Allow-Headers: Content-Type, Authorization');

if ($_SERVER['REQUEST_METHOD'] == 'OPTIONS') {
    exit(0);
}

require_once __DIR__ . '/../../config.php';
require_once __DIR__ . '/../../db.php';

$response = ['status' => 'error', 'message' => 'Invalid request'];
$conn = null;

// Typically PUT is used for updates, but we'll accept POST too for simplicity if needed
if ($_SERVER['REQUEST_METHOD'] === 'PUT' || $_SERVER['REQUEST_METHOD'] === 'POST') { 
    
    // Read JSON data from the request body
    $input = json_decode(file_get_contents('php://input'), true);

    // Basic validation
    if (empty($input['id']) || empty($input['name']) || empty($input['username']) || empty($input['email']) || empty($input['role']) || empty($input['gender']) || !isset($input['age'])) {
        http_response_code(400); // Bad Request
        $response['message'] = 'Missing required fields for update.';
    } else {
        // Sanitize and validate input
        $id = filter_var($input['id'], FILTER_VALIDATE_INT);
        $name = sanitizeInput($input['name']);
        $username = sanitizeInput($input['username']);
        $age = filter_var($input['age'], FILTER_VALIDATE_INT, ['options' => ['min_range' => 0]]);
        $role = sanitizeInput($input['role']);
        $gender = sanitizeInput($input['gender']);
        $email = filter_var($input['email'], FILTER_VALIDATE_EMAIL) ? sanitizeInput($input['email']) : null;
        $webpage = isset($input['webpage']) && filter_var($input['webpage'], FILTER_VALIDATE_URL) ? sanitizeInput($input['webpage']) : null;
        $profile = isset($input['profile']) ? sanitizeInput($input['profile']) : '';

        if ($id === false || $id <= 0 || $age === false || $email === null) {
             http_response_code(400);
             $response['message'] = 'Invalid ID, age, or email format.';
        } else {
            try {
                $conn = getDBConnection();

                // Escape inputs
                $escaped_username = escapeSQL($conn, $username);
                $escaped_email = escapeSQL($conn, $email);

                // Check if username or email already exists for *other* users
                $check_sql = "SELECT id FROM users WHERE (username = ? OR email = ?) AND id != ?";
                $check_stmt = $conn->prepare($check_sql);
                if (!$check_stmt) throw new Exception("Prepare failed (check): " . $conn->error);
                $check_stmt->bind_param("ssi", $escaped_username, $escaped_email, $id);
                $check_stmt->execute();
                $check_result = $check_stmt->get_result();
                
                if ($check_result->num_rows > 0) {
                    http_response_code(409); // Conflict
                    $response['message'] = 'Username or email already exists for another user!';
                } else {
                    // NOTE: Password update is not included here, handle separately if needed
                    $sql = "UPDATE users SET name = ?, username = ?, age = ?, role = ?, gender = ?, 
                            email = ?, webpage = ?, profile = ? WHERE id = ?";
                    $stmt = $conn->prepare($sql);
                    if (!$stmt) throw new Exception("Prepare failed (update): " . $conn->error);

                    // Escape other fields
                    $escaped_name = escapeSQL($conn, $name);
                    $escaped_role = escapeSQL($conn, $role);
                    $escaped_gender = escapeSQL($conn, $gender);
                    $escaped_webpage = $webpage ? escapeSQL($conn, $webpage) : null;
                    $escaped_profile = escapeSQL($conn, $profile);
                    
                    $stmt->bind_param("ssisssssi", $escaped_name, $escaped_username, $age, $escaped_role, $escaped_gender, $escaped_email, $escaped_webpage, $escaped_profile, $id);
                    
                    if ($stmt->execute()) {
                        if ($stmt->affected_rows > 0) {
                           $response['status'] = 'success';
                           $response['message'] = 'User updated successfully!';
                        } else {
                             // Technically successful execution, but no rows changed
                           $response['status'] = 'success'; // Or 'no_change'? 
                           $response['message'] = 'User data was the same or user not found.';
                           // You might return 404 if ID didn't exist, check affected rows
                        }
                    } else {
                        throw new Exception("Execute failed: " . $stmt->error);
                    }
                    $stmt->close();
                }
                
                $check_stmt->close();
                $conn->close();

            } catch (Exception $e) {
                http_response_code(500);
                $response['message'] = "Error updating user: " . $e->getMessage();
                 if ($conn) $conn->close();
            }
        }
    }
} else {
    http_response_code(405); // Method Not Allowed
    $response['message'] = 'PUT or POST method required.';
}

echo json_encode($response);
?> 