<?php

function startClient($host, $port) {
    $socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);

    if (!socket_connect($socket, $host, $port)) {
        echo "Connection failed.\n";
        return;
    }

    echo "Connected to the server. Type 'exit' to quit.\n";

    while (true) {
        $message = readline("Enter a math problem to send (or 'exit' to quit): ");

        if (strtolower(trim($message)) === 'exit') {
            break;
        }

        socket_write($socket, $message, strlen($message));

        $response = socket_read($socket, 1024);
        echo "Server responded: $response\n";

        if ($response === "Invalid expression") {
            echo "Please enter a valid math problem.\n";
        }
    }

    socket_close($socket);
    echo "Connection closed.\n";
}

startClient("172.20.10.10", 5433);

?>