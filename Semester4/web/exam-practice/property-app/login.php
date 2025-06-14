<?php

session_start();
require_once 'db.php';

$error = '';
$step = 1;
$name = '';
$question = '';

// Step 2: Check secret answer
if (isset($_POST['answer'])) {
    $name = $_POST['name'];
    $stmt = $conn->prepare("SELECT * FROM User WHERE name = ?");
    $stmt->bind_param("s", $name);
    $stmt->execute();
    $result = $stmt->get_result()->fetch_assoc();
    $question = $result['secretQuestion'];

    if ($result && $result['secretAnswer'] == $_POST['answer']) {
        $_SESSION['id'] = $result['id'];
        $_SESSION['name'] = $name;
        header('Location: dashboard.php');
        exit();
    } 
    else {
        $error = 'Invalid answer';
        $step = 1;
    }
}
// Step 1: the secret question
elseif (isset($_POST['name'])) {
    $name = $_POST['name'];
    $stmt = $conn->prepare("SELECT * FROM User WHERE name = ?");
    $stmt->bind_param("s", $name);
    $stmt->execute();
    $result = $stmt->get_result()->fetch_assoc();

    if ($result) {
        $question = $result['secretQuestion'];
        $step = 2;
    }
    else {
        $error = 'Invalid name';
        $step = 1;
    }
}

?>

<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Property Manager</title>
</head>
<body>
    <h1>Login</h1>
    <?php if ($error): ?><p style="color: red;"><?php echo $error; ?></p><?php endif; ?>
    <?php if ($step == 1): ?>
        <form method="post" action="login.php">
            <label for="name">Name</label>
            <input type="text" name="name" placeholder="Name" value="<?php echo $name; ?>">
            <button type="submit">Submit</button>
        </form>
    <?php else: ?>
        <form method="post" action="login.php">
            <p>Question: <?php echo htmlspecialchars($question); ?></p>
            <input type="hidden" name="name" value="<?php echo htmlspecialchars($name); ?>">
            <label for="answer">Secret Answer</label>
            <input type="text" name="answer" placeholder="Answer" required>
            <button type="submit">Login</button>
        </form>
    <?php endif; ?>
</body>
<html>
    
