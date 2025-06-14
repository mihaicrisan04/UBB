<?php
session_start();
// Redirect to login if not authenticated
if (!isset($_SESSION['id'])) {
    header('Location: login.php');
    exit();
}

require_once 'db.php';
$userId = $_SESSION['id'];
$page_data = []; // To hold results for display

// --- ACTION HANDLER ---
if ($_SERVER['REQUEST_METHOD'] === 'POST' && isset($_POST['action'])) {
    switch ($_POST['action']) {
        case 'add_property':
            $address = $_POST['address'];
            $description = $_POST['description'];
            // Find or create property
            $stmt = $conn->prepare("SELECT id FROM Property WHERE address = ?");
            $stmt->bind_param("s", $address);
            $stmt->execute();
            $prop = $stmt->get_result()->fetch_assoc();
            if ($prop) {
                $propId = $prop['id'];
            } else {
                $stmt = $conn->prepare("INSERT INTO Property (address, description) VALUES (?, ?)");
                $stmt->bind_param("ss", $address, $description);
                $stmt->execute();
                $propId = $conn->insert_id;
            }
            // Link property to user (ignore if link already exists)
            $stmt = $conn->prepare("INSERT IGNORE INTO UserToProperties (idUser, idProperty) VALUES (?, ?)");
            $stmt->bind_param("ii", $userId, $propId);
            $stmt->execute();
            break;

        case 'delete_property':
            $propId = $_POST['property_id'];

            // Delete the property from the UserToProperties table
            $stmt = $conn->prepare("DELETE FROM UserToProperties WHERE idUser = ? AND idProperty = ?");
            $stmt->bind_param("ii", $userId, $propId);
            $stmt->execute();

            // Check if the property is still owned by any other user
            $stmt = $conn->prepare("SELECT COUNT(*) FROM UserToProperties WHERE idProperty = ?");
            $stmt->bind_param("i", $propId);
            $stmt->execute();
            $result = $stmt->get_result()->fetch_assoc();
            if ($result['COUNT(*)'] == 0) {
                // Delete the property from the Property table
                $stmt = $conn->prepare("DELETE FROM Property WHERE id = ?");
                $stmt->bind_param("i", $propId);
                $stmt->execute();
            }

            break;

        case 'search':
            $query = "%" . $_POST['search_query'] . "%";
            $stmt = $conn->prepare("SELECT * FROM Property WHERE description LIKE ?");
            $stmt->bind_param("s", $query);
            $stmt->execute();
            $results = $stmt->get_result()->fetch_all(MYSQLI_ASSOC);
            $page_data['search_results'] = $results;
            // Store search hits for the "popular" feature
            if (!isset($_SESSION['search_hits'])) $_SESSION['search_hits'] = [];
            foreach ($results as $prop) {
                $_SESSION['search_hits'][] = $prop['id'];
            }
            break;

        case 'show_popular':
            if (!empty($_SESSION['search_hits'])) {
                $counts = array_count_values($_SESSION['search_hits']);
                arsort($counts);
                $popularId = key($counts);
                $stmt = $conn->prepare("SELECT * FROM Property WHERE id = ?");
                $stmt->bind_param("i", $popularId);
                $stmt->execute();
                $page_data['popular_property'] = $stmt->get_result()->fetch_assoc();
            }
            break;

        case 'view_multi_owner':
            $sql = "SELECT p.*, COUNT(utp.idUser) as owner_count
                    FROM Property p
                    JOIN UserToProperties utp ON p.id = utp.idProperty
                    GROUP BY p.id
                    HAVING owner_count > 1";
            $page_data['multi_owner_properties'] = $conn->query($sql)->fetch_all(MYSQLI_ASSOC);
            break;
    }
}

// --- DATA FOR DISPLAY ---
// Always fetch the user's current properties
$stmt = $conn->prepare("SELECT p.* FROM Property p JOIN UserToProperties utp ON p.id = utp.idProperty WHERE utp.idUser = ?");
$stmt->bind_param("i", $userId);
$stmt->execute();
$my_properties = $stmt->get_result()->fetch_all(MYSQLI_ASSOC);
?>

<!DOCTYPE html>
<html>
<head><title>Dashboard</title></head>
<body>
    <h1>Welcome, <?php echo htmlspecialchars($_SESSION['name']); ?>!</h1>
    <a href="logout.php">Logout</a>
    <hr>

    <!-- Add/Claim Property -->
    <h2>Add or Claim a Property</h2>
    <form action="dashboard.php" method="post">
        <input type="hidden" name="action" value="add_property">
        <input type="text" name="address" placeholder="Property Address" required>
        <textarea name="description" placeholder="Description"></textarea>
        <button type="submit">Add/Claim</button>
    </form>
    <hr>

    <!-- My Properties -->
    <h2>My Properties</h2>
    <?php if (empty($my_properties)): ?>
        <p>You do not own any properties.</p>
    <?php else: ?>
        <ul>
            <?php foreach ($my_properties as $prop): ?>
                <li>
                    <?php echo htmlspecialchars($prop['address']); ?> (<?php echo htmlspecialchars($prop['description']); ?>)
                    <form action="dashboard.php" method="post" style="display:inline;">
                        <input type="hidden" name="action" value="delete_property">
                        <input type="hidden" name="property_id" value="<?php echo $prop['id']; ?>">
                        <button type="submit">Delete</button>
                    </form>
                </li>
            <?php endforeach; ?>
        </ul>
    <?php endif; ?>
    <hr>

    <!-- Search & Other Actions -->
    <h2>Actions</h2>
    <form action="dashboard.php" method="post" style="display:inline-block;">
        <input type="hidden" name="action" value="search">
        <input type="text" name="search_query" placeholder="Search description">
        <button type="submit">Search</button>
    </form>
    <form action="dashboard.php" method="post" style="display:inline-block;">
        <input type="hidden" name="action" value="show_popular">
        <button type="submit">Show Most Popular</button>
    </form>
    <form action="dashboard.php" method="post" style="display:inline-block;">
        <input type="hidden" name="action" value="view_multi_owner">
        <button type="submit">View Multi-Owner Properties</button>
    </form>
    <hr>

    <!-- Results Display Area -->
    <?php if (isset($page_data['search_results'])): ?>
        <h2>Search Results</h2>
        <ul><?php foreach ($page_data['search_results'] as $p) echo "<li>" . htmlspecialchars($p['address']) . "</li>"; ?></ul>
    <?php elseif (isset($page_data['popular_property'])): ?>
        <h2>Most Popular Searched Property</h2>
        <p><?php echo htmlspecialchars($page_data['popular_property']['address']); ?></p>
    <?php elseif (isset($page_data['multi_owner_properties'])): ?>
        <h2>Multi-Owner Properties</h2>
        <ul><?php foreach ($page_data['multi_owner_properties'] as $p) echo "<li>" . htmlspecialchars($p['address']) . "</li>"; ?></ul>
    <?php endif; ?>
</body>
</html>