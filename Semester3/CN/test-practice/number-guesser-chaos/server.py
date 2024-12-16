import socket
import threading
import time
import random
import struct


port_tcp = 2222
port_udp = 1234
# host = 'localhost'
host = '0.0.0.0'

# global variables
random.seed()
my_lock = threading.Lock()
e = threading.Event()
e.clear()
threads = []
winner_thread = 0
client_count = 0
client_guessed = False
my_num = random.randint(1, 10) 

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

def client_worker(client_socket):
    global client_count, client_guessed, my_num, winner_thread

    print(f"Client connected: {client_socket.getpeername()}")
    message = f"Welcome to the number guesser client {client_count}!"
    client_socket.sendall(message.encode('utf-8'))

    while not client_guessed:
        try:
            data = client_socket.recv(4) # receive 4 bytes as the clients guess
            cnumber = struct.unpack('!I', data)[0] # unpack the 4 bytes into an integer
            print(f"Client {client_socket.getpeername()} guessed: {cnumber}")

            if cnumber > my_num:
                client_socket.sendall(b'S')
            if cnumber < my_num:
                client_socket.sendall(b'H')
            if cnumber == my_num:
                my_lock.acquire()
                client_guessed = True
                winner_thread = threading.get_ident()
                my_lock.release()

        except socket.error as msg:
            print('Error:', msg.strerror)
            break
    
    if client_guessed:
        if threading.get_ident() == winner_thread:
            client_socket.sendall(b'G')
            print(f"Client: {client_socket.getpeername()} is the winner!")
            e.set()
        else:
            client_socket.sendall(b'L')
            print(f"Client {client_socket.getpeername()} is the loser")

    time.sleep(1)
    client_socket.close()

def reset_server():
    global threads, client_count, client_guessed, winner_thread, my_num

    while True:
        e.wait()
        for t in threads:
            t.join()
        print("all threads are finished now")
        e.clear()

        my_lock.acquire()

        threads = []
        client_count = 0
        client_guessed = False
        my_num = random.randint(1, 10)
        winner_thread = -1

        my_lock.release()

def accept_clients(tcp_sock):
    global client_count, threads

    while True:
        client_socket, addr = tcp_sock.accept() # wait for a connection
        t = threading.Thread(target=client_worker, args=(client_socket,))
        threads.append(t)
        client_count += 1
        t.start()

def main():
    tcp_sock = setup_tcp()
    # udp_sock = setup_udp()

    t = threading.Thread(target=reset_server, daemon=True)
    t.start()

    accept_clients(tcp_sock)


main()
