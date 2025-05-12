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

$response = [];
$conn = null;

try {
    $conn = getDBConnection();

    $nameQuery = isset($_GET['name']) ? sanitizeInput($_GET['name']) : '';

    // Select relevant fields, exclude password
    $sql = "SELECT id, name, username, age, role, gender, email, webpage, profile, created_at FROM users";
    $params = [];
    $types = '';

    if (!empty($nameQuery)) {
        // Use LIKE for partial matching
        $sql .= " WHERE name LIKE ? OR username LIKE ?"; 
        $searchTerm = '%' . $conn->real_escape_string($nameQuery) . '%'; // Escape for LIKE
        $params[] = $searchTerm;
        $params[] = $searchTerm;
        $types .= 'ss';
    }
    $sql .= " ORDER BY name ASC";

    $stmt = $conn->prepare($sql);
    if (!$stmt) {
        throw new Exception("Error preparing statement: " . $conn->error);
    }

    if (!empty($params)) {
        $stmt->bind_param($types, ...$params);
    }

    $stmt->execute();
    $result = $stmt->get_result();
    
    $users = [];
    if ($result->num_rows > 0) {
        while($row = $result->fetch_assoc()) {
            $users[] = $row;
        }
    }
    
    $response['status'] = 'success';
    $response['users'] = $users;

    $stmt->close();
    $conn->close();

} catch (Exception $e) {
    http_response_code(500);
    $response['status'] = 'error';
    $response['message'] = "Error searching users: " . $e->getMessage();
    if ($conn) $conn->close();
}

echo json_encode($response);
?> 