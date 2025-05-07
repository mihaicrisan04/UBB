<?php
require_once 'config.php';
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Browse Users - User Management System</title>
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
            <h2>Browse Users</h2>
            
            <div class="filter-section">
                <p>Previous filter: <span id="previous-filter">None</span></p>
                <label for="role-filter">Filter by Role:</label>
                <select id="role-filter" onchange="filterUsers()">
                    <option value="">All Roles</option>
                    <option value="admin">Admin</option>
                    <option value="user">User</option>
                    <option value="manager">Manager</option>
                    <option value="employee">Employee</option>
                </select>
            </div>
            
            <div id="users-table">
                <!-- Users will be loaded here via AJAX -->
            </div>
        </main>
        
        <footer>
            <p>&copy; 2024 User Management System</p>
        </footer>
    </div>
    
    <script>
        let previousFilter = 'None';
        
        function filterUsers() {
            const role = document.getElementById('role-filter').value;
            const xhr = new XMLHttpRequest();
            
            // Update the previous filter display
            document.getElementById('previous-filter').textContent = previousFilter;
            
            // Store current filter as previous for next time
            previousFilter = role ? role : 'All Roles';
            
            xhr.onreadystatechange = function() {
                if (this.readyState === 4 && this.status === 200) {
                    document.getElementById('users-table').innerHTML = this.responseText;
                }
            };
            
            xhr.open('GET', 'ajax/get_users.php?role=' + encodeURIComponent(role), true);
            xhr.send();
        }
        
        function deleteUser(id) {
            if (confirm('Are you sure you want to delete this user?')) {
                const xhr = new XMLHttpRequest();
                
                xhr.onreadystatechange = function() {
                    if (this.readyState === 4 && this.status === 200) {
                        filterUsers(); // Refresh the table
                    }
                };
                
                xhr.open('POST', 'ajax/delete_user.php', true);
                xhr.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
                xhr.send('id=' + id);
            }
        }
        
        // Load users when page loads
        window.onload = filterUsers;
    </script>
</body>
</html>