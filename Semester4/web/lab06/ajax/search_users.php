<?php
require_once '../config.php';
require_once '../db.php';

header('Content-Type: text/html');

$conn = getDBConnection();

$name = isset($_GET['name']) ? sanitizeInput($_GET['name']) : '';

$sql = "SELECT * FROM users";
if (!empty($name)) {
    $sql .= " WHERE name LIKE ?";
    $name = "%$name%";
}

$stmt = $conn->prepare($sql);
if (!empty($name)) {
    $stmt->bind_param("s", $name);
}

$stmt->execute();
$result = $stmt->get_result();

if ($result->num_rows > 0) {
    echo '<table>';
    echo '<tr>
            <th>Name</th>
            <th>Username</th>
            <th>Role</th>
            <th>Email</th>
            <th>Age</th>
            <th>Actions</th>
          </tr>';
    
    while ($row = $result->fetch_assoc()) {
        echo '<tr>';
        echo '<td>' . htmlspecialchars($row['name']) . '</td>';
        echo '<td>' . htmlspecialchars($row['username']) . '</td>';
        echo '<td>' . htmlspecialchars($row['role']) . '</td>';
        echo '<td>' . htmlspecialchars($row['email']) . '</td>';
        echo '<td>' . htmlspecialchars($row['age']) . '</td>';
        echo '<td>
                <a href="edit.php?id=' . $row['id'] . '">Edit</a> |
                <a href="#" onclick="deleteUser(' . $row['id'] . ')">Delete</a>
              </td>';
        echo '</tr>';
    }
    
    echo '</table>';
} else {
    echo '<p>No users found matching your search criteria.</p>';
}

$stmt->close();
$conn->close();
?> 