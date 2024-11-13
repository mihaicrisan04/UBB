import socket
import threading
import random
import re

def evaluate_expression(expression):
    try:
        result = eval(expression)
        return str(result)
    except Exception:
        return "Invalid expression"

def handle_client(client_socket: socket.socket, addr: str):
    try:
        while True:
            message = client_socket.recv(1024).decode('utf-8').strip()
            if not message:
                break
            print(f"Received from {addr}: {message}")

            if not re.match(r'^\s*\d+\s*[\+\-\*/]\s*\d+\s*$', message):
                response = "Invalid expression"
                client_socket.send(response.encode('utf-8'))
                continue

            correct_result = evaluate_expression(message)
            troll = random.choice([True, False]) 

            if troll:
                fake_result = random.choice(["banana", f"{random.randint(1,20)} trust me, I did the math"])
                response = f"{message} = {fake_result}"
                print(f"Trolling {addr} with: {response}")
            else:
                response = f"{message} = {correct_result}"
                print(f"Sending to {addr}: {response}")

            client_socket.send(response.encode('utf-8'))
    finally:
        client_socket.close()
        print(f"Connection closed with {addr}")

def start_server(host, port):
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind((host, port))
    server.listen(5)
    print(f"Server listening on {host}:{port}")

    while True:
        client_socket, addr = server.accept()
        print(f"Accepted connection from {addr}")

        client_thread = threading.Thread(target=handle_client, args=(client_socket, addr))
        client_thread.start()


if __name__ == "__main__":
    start_server('0.0.0.0', 5433)