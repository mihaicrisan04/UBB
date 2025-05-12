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

    $roleFilter = isset($_GET['role']) ? sanitizeInput($_GET['role']) : '';

    $sql = "SELECT id, name, username, age, role, gender, email, webpage, profile, created_at FROM users";
    $params = [];
    $types = '';

    if (!empty($roleFilter)) {
        $sql .= " WHERE role = ?";
        $params[] = $roleFilter;
        $types .= 's';
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
            // Sanitize output for safety, though less critical for data going to a trusted frontend framework
            // $row['name'] = htmlspecialchars($row['name']); 
            // ... etc. for other fields if displaying directly as HTML somewhere insecure
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
    $response['message'] = "Error fetching users: " . $e->getMessage();
    if ($conn) $conn->close();
}

echo json_encode($response);
?> 