<?php
header('Content-Type: application/json');
header('Access-Control-Allow-Origin: *');
header('Access-Control-Allow-Methods: DELETE, POST, OPTIONS'); // Allow DELETE (or POST if preferred)
header('Access-Control-Allow-Headers: Content-Type, Authorization');

if ($_SERVER['REQUEST_METHOD'] == 'OPTIONS') {
    exit(0);
}

require_once __DIR__ . '/../../config.php';
require_once __DIR__ . '/../../db.php';

$response = ['status' => 'error', 'message' => 'Invalid request'];
$conn = null;

// Use DELETE method, but also check for id in POST for broader compatibility if needed.
// For a strict REST API, only DELETE should be used.
if ($_SERVER['REQUEST_METHOD'] === 'DELETE' || ($_SERVER['REQUEST_METHOD'] === 'POST' && isset($_POST['id']))) { 
    
    $id = null;
    if ($_SERVER['REQUEST_METHOD'] === 'DELETE') {
        // For DELETE, ID might come as a query param e.g., /api/delete_user.php?id=X
        // Or from a JSON body, depending on how the frontend sends it.
        if (isset($_GET['id'])) {
            $id = filter_var($_GET['id'], FILTER_VALIDATE_INT);
        } else {
            // Try to get from JSON body if Content-Type is application/json
            $input = json_decode(file_get_contents('php://input'), true);
            if (isset($input['id'])) {
                $id = filter_var($input['id'], FILTER_VALIDATE_INT);
            }
        }
    } else { // POST request
        $id = filter_var($_POST['id'], FILTER_VALIDATE_INT);
    }

    if ($id === false || $id <= 0) {
        http_response_code(400);
        $response['message'] = 'Invalid or missing User ID.';
    } else {
        try {
            $conn = getDBConnection();
            
            $sql = "DELETE FROM users WHERE id = ?";
            $stmt = $conn->prepare($sql);
            if (!$stmt) throw new Exception("Prepare failed: " . $conn->error);
            
            $stmt->bind_param("i", $id);
            
            if ($stmt->execute()) {
                if ($stmt->affected_rows > 0) {
                    $response['status'] = 'success';
                    $response['message'] = 'User deleted successfully!';
                } else {
                    http_response_code(404); // Not Found
                    $response['message'] = 'User not found or already deleted.';
                }
            } else {
                throw new Exception("Execute failed: " . $stmt->error);
            }
            
            $stmt->close();
            $conn->close();

        } catch (Exception $e) {
            http_response_code(500);
            $response['message'] = "Error deleting user: " . $e->getMessage();
            if ($conn) $conn->close();
        }
    }
} else {
    http_response_code(405); // Method Not Allowed
    $response['message'] = 'DELETE or POST (with id) method required.';
}

echo json_encode($response);
?> 