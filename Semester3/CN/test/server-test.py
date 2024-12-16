import socket
import threading


def handle_client(client_socket,addr):
    try:
        while True:
            message = client_socket.recv(1024).decode('utf-8')
            if not message:
                break
            print(f"Received: {message}")
            client_socket.send(f"Echo: {message}".encode('utf-8'))
    finally:
        client_socket.close()
        print("Connection closed for client: ", addr)


def start_server(host, port):
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind((host, port))
    server.listen(5)
    print(f"Server listening on {host}:{port}")

    while True:
        client_socket, addr = server.accept()
        print(f"Accepted connection from {addr}")

        client_thread = threading.Thread(target=handle_client, args=(client_socket,addr))
        client_thread.start()


if __name__ == "__main__":
    start_server('0.0.0.0', 9876)