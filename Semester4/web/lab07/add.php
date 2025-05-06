<?php
require_once 'config.php';
require_once 'db.php';

$message = '';

if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    $name = sanitizeInput($_POST['name']);
    $username = sanitizeInput($_POST['username']);
    $password = password_hash($_POST['password'], PASSWORD_DEFAULT);
    $age = (int)sanitizeInput($_POST['age']);
    $role = sanitizeInput($_POST['role']);
    $email = sanitizeInput($_POST['email']);
    $webpage = sanitizeInput($_POST['webpage']);
    $profile = sanitizeInput($_POST['profile']);
    
    $conn = getDBConnection();
    
    // Check if username or email already exists
    $check_sql = "SELECT id FROM users WHERE username = ? OR email = ?";
    $check_stmt = $conn->prepare($check_sql);
    $check_stmt->bind_param("ss", $username, $email);
    $check_stmt->execute();
    $check_result = $check_stmt->get_result();
    
    if ($check_result->num_rows > 0) {
        $message = '<div class="error">Username or email already exists!</div>';
    } else {
        $sql = "INSERT INTO users (name, username, password, age, role, email, webpage, profile) 
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)";
        $stmt = $conn->prepare($sql);
        $stmt->bind_param("sssissss", $name, $username, $password, $age, $role, $email, $webpage, $profile);
        
        if ($stmt->execute()) {
            $message = '<div class="success">User added successfully!</div>';
        } else {
            $message = '<div class="error">Error adding user!</div>';
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
    <title>Add User - User Management System</title>
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
            <h2>Add New User</h2>
            
            <?php echo $message; ?>
            
            <form method="POST" onsubmit="return validateForm()">
                <div class="form-group">
                    <label for="name">Name:</label>
                    <input type="text" id="name" name="name" required>
                </div>
                
                <div class="form-group">
                    <label for="username">Username:</label>
                    <input type="text" id="username" name="username" required>
                </div>
                
                <div class="form-group">
                    <label for="password">Password:</label>
                    <input type="password" id="password" name="password" required>
                </div>
                
                <div class="form-group">
                    <label for="age">Age:</label>
                    <input type="number" id="age" name="age" min="1" max="120" required>
                </div>
                
                <div class="form-group">
                    <label for="role">Role:</label>
                    <select id="role" name="role" required>
                        <option value="">Select Role</option>
                        <option value="admin">Admin</option>
                        <option value="user">User</option>
                        <option value="manager">Manager</option>
                        <option value="employee">Employee</option>
                    </select>
                </div>
                
                <div class="form-group">
                    <label for="email">Email:</label>
                    <input type="email" id="email" name="email" required>
                </div>
                
                <div class="form-group">
                    <label for="webpage">Webpage:</label>
                    <input type="url" id="webpage" name="webpage">
                </div>
                
                <div class="form-group">
                    <label for="profile">Profile:</label>
                    <textarea id="profile" name="profile" rows="4"></textarea>
                </div>
                
                <button type="submit">Add User</button>
            </form>
        </main>
        
        <footer>
            <p>&copy; 2024 User Management System</p>
        </footer>
    </div>
    
    <script>
        function validateForm() {
            const name = document.getElementById('name').value;
            const username = document.getElementById('username').value;
            const password = document.getElementById('password').value;
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
            
            if (password.length < 6) {
                alert('Password must be at least 6 characters long');
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