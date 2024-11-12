'''
Server is the auction-house, tries to sell something. Base price is a random between 50-100.
The server handles multiple clients connecting at the same time to the auction house.
Each client will send his next bid, through TCP (each bid will be higher than the current price).
The server updates the current price with the bid.
The auction ends when the server has not received a bid in the last 3 seconds, and the client with the last bid is declared a winner.
Server sends through UDP periodically the current price
'''

# TODO reset the auction after it ends

import socket
import threading
import random
import time
import struct


port_udp = 1234
port_tcp = 2222
host = '0.0.0.0'

random.seed()
# current_price = random.randint(50, 100)
current_price = 50
lock = threading.Lock()
threads = []
winner_thread = 0
finished = False
last_bid_time = time.time()

def error(message):
    print(message)
    exit(1)

def is_auction_active():
    global last_bid_time

    return True
    # return time.time() - last_bid_time < 7

def broadcast_price():
    global current_price

    with lock:
        udp_sock.sendto(struct.pack('I', current_price), ('<broadcast>', port_udp))

def setup_udp():
    udp_sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    udp_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    udp_sock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)

    print(f"UDP Running on {host}:{port_udp}")

    return udp_sock

def setup_tcp():
    tcp_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    tcp_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

    tcp_sock.bind((host, port_tcp))
    tcp_sock.listen(5) # listen for at most 5 connections

    print(f"TCP Running on {host}:{port_tcp}")

    return tcp_sock

def recv_all(sock, length):
    data = b''
    while len(data) < length:
        more = sock.recv(length - len(data))
        if not more:
            raise EOFError("Socket closed before receiving all data")
        data += more
    return data

def client_worker(cs):
    global current_price, winner_thread, last_bid_time, lock

    message = f"You are connected to the auction house. Current price is {current_price}"
    cs.sendall(message.encode('utf-8'))

    while is_auction_active():
        try:
            # data = cs.recv(4) # wait for the client to send a bid
            data = recv_all(cs, 4)  # Ensure we receive exactly 4 bytes
            bid = struct.unpack('I', data)[0]
            print(f"Client {cs.getpeername()} bid: {bid}")

            if bid > current_price:
                with lock:
                    current_price = bid
                    winner_thread = threading.get_ident()
                    last_bid_time = time.time()
                broadcast_price()
                print(f"New price: {current_price}")
                cs.sendall(b's')
            else:
                cs.sendall(b'f')

        except Exception as e:
            print(f"Error: {e}")
            break


    if threading.get_ident() == winner_thread:
        cs.sendall(b'w')
        print(f"Client {cs.getpeername()} is the winner!")
    else:
        cs.sendall(b'l')

    cs.close()

def cleanup():
    global threads

    for t in threads:
        t.join()

    udp_sock.close()
    tcp_sock.close()

def reset_server():
    pass

def accept_clients():
    global tcp_sock, threads

    while is_auction_active():
        client_socket, client_addr = tcp_sock.accept() # wait for a client to connect
        print(f"Client connected: {client_addr}")
        client_thread = threading.Thread(target=client_worker, args=(client_socket,))
        threads.append(client_thread)
        client_thread.start()


if __name__ == "__main__":
    udp_sock = setup_udp()
    tcp_sock = setup_tcp()

    tcp_thread = threading.Thread(target=accept_clients)
    tcp_thread.start()

    while is_auction_active():
        broadcast_price()
        time.sleep(1)

    cleanup()
    tcp_thread.join()



