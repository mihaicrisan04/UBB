import socket


def start_client(host, port):
    client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client.connect((host, port))

    try:
        while True:
            message = input("Enter message to send (or 'exit' to quit): ")
            if message.lower() == 'exit':
                break

            client.send(message.encode('utf-8'))

            response = client.recv(1024).decode('utf-8')
            print(f"Received: {response}")
    finally:
        client.close()


if __name__ == "__main__":
    start_client('192.168.0.101', 8000)