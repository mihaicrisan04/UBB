import socket
import threading
import struct

# Auction data
current_price = 100  # Starting price
clients = []  # List to hold connected TCP clients

# TCP and UDP Server Setup
tcp_port = 65432
udp_port = 65433
udp_broadcast_ip = '255.255.255.255'  # Broadcast IP for UDP

def handle_client(connection, address):
    global current_price
    print(f"New connection from {address}")

    try:
        while True:
            # Receive a bid from the client
            data = connection.recv(1024).decode()
            if not data:
                break
            new_bid = int(data)
            print(f"Received bid of {new_bid} from {address}")

            # Update current price if the new bid is higher
            if new_bid > current_price:
                current_price = new_bid
                print(f"New highest bid: {current_price}")
                broadcast_price()
            else:
                connection.sendall(b"Bid too low\n")

    finally:
        connection.close()
        print(f"Connection with {address} closed")

def broadcast_price():
    # Create a UDP socket for broadcasting
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as udp_socket:
        udp_socket.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
        message = struct.pack("!I", current_price)  # Pack price as 4-byte integer big-endian
        udp_socket.sendto(message, (udp_broadcast_ip, udp_port))
        print(f"Broadcasted new price: {current_price}")

def start_tcp_server():
    # Start TCP server
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as tcp_socket:
        tcp_socket.bind(('', tcp_port))
        tcp_socket.listen()
        print(f"TCP server listening on port {tcp_port}")

        while True:
            connection, address = tcp_socket.accept()
            clients.append(connection)
            client_thread = threading.Thread(target=handle_client, args=(connection, address))
            client_thread.start()

if __name__ == "__main__":
    # Start the TCP server in a separate thread
    tcp_thread = threading.Thread(target=start_tcp_server)
    tcp_thread.start()
    print("Auction house server started")