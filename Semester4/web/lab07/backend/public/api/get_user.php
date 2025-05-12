<?php
header('Content-Type: application/json');
header('Access-Control-Allow-Origin: *');
header('Access-Control-Allow-Methods: GET, OPTIONS');
header('Access-Control-Allow-Headers: Content-Type, Authorization');

if ($_SERVER['REQUEST_METHOD'] == 'OPTIONS') {
    exit(0);
}

require_once __DIR__ . '/../../config.php';
require_once __DIR__ . '/../../db.php';

$response = ['status' => 'error', 'message' => 'User ID not provided'];
$conn = null;

if (isset($_GET['id'])) {
    $id = filter_var($_GET['id'], FILTER_VALIDATE_INT);

    if ($id === false || $id <= 0) {
        http_response_code(400);
        $response['message'] = 'Invalid User ID.';
    } else {
        try {
            $conn = getDBConnection();
            
            // Fetch user, excluding password hash
            $sql = "SELECT id, name, username, age, role, gender, email, webpage, profile FROM users WHERE id = ?";
            $stmt = $conn->prepare($sql);
            if (!$stmt) throw new Exception("Prepare failed: " . $conn->error);

            $stmt->bind_param("i", $id);
            $stmt->execute();
            $result = $stmt->get_result();
            
            if ($result->num_rows > 0) {
                $user = $result->fetch_assoc();
                $response['status'] = 'success';
                $response['user'] = $user;
                unset($response['message']); // Remove default error message
            } else {
                http_response_code(404); // Not Found
                $response['message'] = 'User not found.';
            }
            
            $stmt->close();
            $conn->close();

        } catch (Exception $e) {
            http_response_code(500);
            $response['message'] = "Error fetching user: " . $e->getMessage();
             if ($conn) $conn->close();
        }
    }
} else {
    http_response_code(400); // Bad Request if ID is missing
}

echo json_encode($response);
?> 