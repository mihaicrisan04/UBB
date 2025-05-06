<?php
require_once 'config.php';
require_once 'db.php';

// Check if database is set up
$needsSetup = !tableExists('users');
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>User Management System</title>
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
            <h2>Welcome to the User Management System</h2>
            
            <?php if ($needsSetup): ?>
            <div class="setup-notice">
                <p>The database needs to be set up before you can use the system.</p>
                <a href="setup.php" class="setup-button">Run Setup</a>
            </div>
            <?php else: ?>
            <p>This system allows you to manage users in an enterprise environment. You can:</p>
            <ul>
                <li>Browse users by role</li>
                <li>Search for users by name</li>
                <li>Add new users</li>
                <li>Edit existing users</li>
                <li>Delete users</li>
            </ul>
            <?php endif; ?>
        </main>
        
        <footer>
            <p>&copy; 2024 User Management System</p>
        </footer>
    </div>
    <script src="js/main.js"></script>
</body>
</html> 