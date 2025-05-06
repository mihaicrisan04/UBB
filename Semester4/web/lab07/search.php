<?php
require_once 'config.php';
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Search Users - User Management System</title>
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
            <h2>Search Users</h2>
            
            <div class="search-section">
                <label for="search-name">Search by Name:</label>
                <input type="text" id="search-name" onkeyup="searchUsers()" placeholder="Enter name to search...">
            </div>
            
            <div id="search-results">
                <!-- Search results will be loaded here via AJAX -->
            </div>
        </main>
        
        <footer>
            <p>&copy; 2024 User Management System</p>
        </footer>
    </div>
    
    <script>
        function searchUsers() {
            const name = document.getElementById('search-name').value;
            const xhr = new XMLHttpRequest();
            
            xhr.onreadystatechange = function() {
                if (this.readyState === 4 && this.status === 200) {
                    document.getElementById('search-results').innerHTML = this.responseText;
                }
            };
            
            xhr.open('GET', 'ajax/search_users.php?name=' + encodeURIComponent(name), true);
            xhr.send();
        }
        
        // Initial search with empty string to show all users
        window.onload = searchUsers;
    </script>
</body>
</html> 