<?php
require_once 'config.php';
require_once 'db.php';

$message = '';
$user = null;

if (isset($_GET['id'])) {
    $id = (int)sanitizeInput($_GET['id']);
    
    $conn = getDBConnection();
    $sql = "SELECT * FROM users WHERE id = ?";
    $stmt = $conn->prepare($sql);
    $stmt->bind_param("i", $id);
    $stmt->execute();
    $result = $stmt->get_result();
    
    if ($result->num_rows > 0) {
        $user = $result->fetch_assoc();
    } else {
        $message = '<div class="error">User not found!</div>';
    }
    
    $stmt->close();
    $conn->close();
}

if ($_SERVER['REQUEST_METHOD'] === 'POST' && isset($_POST['id'])) {
    $id = (int)sanitizeInput($_POST['id']);
    $name = sanitizeInput($_POST['name']);
    $username = sanitizeInput($_POST['username']);
    $age = (int)sanitizeInput($_POST['age']);
    $role = sanitizeInput($_POST['role']);
    $email = sanitizeInput($_POST['email']);
    $webpage = sanitizeInput($_POST['webpage']);
    $profile = sanitizeInput($_POST['profile']);
    
    $conn = getDBConnection();
    
    // Check if username or email already exists for other users
    $check_sql = "SELECT id FROM users WHERE (username = ? OR email = ?) AND id != ?";
    $check_stmt = $conn->prepare($check_sql);
    $check_stmt->bind_param("ssi", $username, $email, $id);
    $check_stmt->execute();
    $check_result = $check_stmt->get_result();
    
    if ($check_result->num_rows > 0) {
        $message = '<div class="error">Username or email already exists!</div>';
    } else {
        $sql = "UPDATE users SET name = ?, username = ?, age = ?, role = ?, 
                email = ?, webpage = ?, profile = ? WHERE id = ?";
        $stmt = $conn->prepare($sql);
        $stmt->bind_param("ssissssi", $name, $username, $age, $role, $email, $webpage, $profile, $id);
        
        if ($stmt->execute()) {
            $message = '<div class="success">User updated successfully!</div>';
            // Refresh user data
            $user = [
                'id' => $id,
                'name' => $name,
                'username' => $username,
                'age' => $age,
                'role' => $role,
                'email' => $email,
                'webpage' => $webpage,
                'profile' => $profile
            ];
        } else {
            $message = '<div class="error">Error updating user!</div>';
        }
        
        $stmt->close();
    }
    
    $check_stmt->close();
    $conn->close();
}
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Edit User - User Management System</title>
    <link rel="stylesheet" href="css/style.css">
</head>
<body>
    <div class="container">
        <header>
            <h1>User Management System</h1>
            <nav>
                <ul>
                    <li><a href="index.php">Home</a></li>
                    <li><a href="browse.php">Browse Users</a></li>
                    <li><a href="add.php">Add User</a></li>
                    <li><a href="search.php">Search Users</a></li>
                </ul>
            </nav>
        </header>
        
        <main>
            <h2>Edit User</h2>
            
            <?php echo $message; ?>
            
            <?php if ($user): ?>
            <form method="POST" onsubmit="return validateForm()">
                <input type="hidden" name="id" value="<?php echo htmlspecialchars($user['id']); ?>">
                
                <div class="form-group">
                    <label for="name">Name:</label>
                    <input type="text" id="name" name="name" value="<?php echo htmlspecialchars($user['name']); ?>" required>
                </div>
                
                <div class="form-group">
                    <label for="username">Username:</label>
                    <input type="text" id="username" name="username" value="<?php echo htmlspecialchars($user['username']); ?>" required>
                </div>
                
                <div class="form-group">
                    <label for="age">Age:</label>
                    <input type="number" id="age" name="age" value="<?php echo htmlspecialchars($user['age']); ?>" min="1" max="120" required>
                </div>
                
                <div class="form-group">
                    <label for="role">Role:</label>
                    <select id="role" name="role" required>
                        <option value="">Select Role</option>
                        <option value="admin" <?php echo $user['role'] === 'admin' ? 'selected' : ''; ?>>Admin</option>
                        <option value="user" <?php echo $user['role'] === 'user' ? 'selected' : ''; ?>>User</option>
                        <option value="manager" <?php echo $user['role'] === 'manager' ? 'selected' : ''; ?>>Manager</option>
                        <option value="employee" <?php echo $user['role'] === 'employee' ? 'selected' : ''; ?>>Employee</option>
                    </select>
                </div>
                
                <div class="form-group">
                    <label for="email">Email:</label>
                    <input type="email" id="email" name="email" value="<?php echo htmlspecialchars($user['email']); ?>" required>
                </div>
                
                <div class="form-group">
                    <label for="webpage">Webpage:</label>
                    <input type="url" id="webpage" name="webpage" value="<?php echo htmlspecialchars($user['webpage']); ?>">
                </div>
                
                <div class="form-group">
                    <label for="profile">Profile:</label>
                    <textarea id="profile" name="profile" rows="4"><?php echo htmlspecialchars($user['profile']); ?></textarea>
                </div>
                
                <button type="submit">Update User</button>
            </form>
            <?php else: ?>
            <p>User not found.</p>
            <?php endif; ?>
        </main>
        
        <footer>
            <p>&copy; 2024 User Management System</p>
        </footer>
    </div>
    
    <script>
        function validateForm() {
            const name = document.getElementById('name').value;
            const username = document.getElementById('username').value;
            const age = document.getElementById('age').value;
            const role = document.getElementById('role').value;
            const email = document.getElementById('email').value;
            const webpage = document.getElementById('webpage').value;
            
            // Basic validation
            if (name.length < 2) {
                alert('Name must be at least 2 characters long');
                return false;
            }
            
            if (username.length < 3) {
                alert('Username must be at least 3 characters long');
                return false;
            }
            
            if (age < 1 || age > 120) {
                alert('Age must be between 1 and 120');
                return false;
            }
            
            if (!role) {
                alert('Please select a role');
                return false;
            }
            
            if (!email.match(/^[^\s@]+@[^\s@]+\.[^\s@]+$/)) {
                alert('Please enter a valid email address');
                return false;
            }
            
            if (webpage && !webpage.match(/^https?:\/\/.+/)) {
                alert('Please enter a valid URL (starting with http:// or https://)');
                return false;
            }
            
            return true;
        }
    </script>
</body>
</html> 