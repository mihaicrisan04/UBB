import socket
import struct
import random
import sys
import time

port_tcp = 2222
port_udp = 1234 
# host = '172.0.0.1'
host = '172.20.10.10'

random.seed()
finished = False
sr, er = 1, 2**17-1
step_count = 0

def run(s):
    global finished, sr, er, step_count

    while not finished:
        guess = random.randint(sr, er)

        try:
            # s.sendall(guess.encode('utf-8'))
            s.sendall(struct.pack('!I', guess)) # alternative way to send the guess
            # s.sendall(guess.to_bytes(4, byteorder='big')) # alternative way to send the guess 
            answer = s.recv(1)
        except socket.error as msg:
            print('Error: ', msg.strerror)
            s.close()
            exit(-2)

        step_count += 1
        print('Sent ', guess, ' Answer ', answer.decode('ascii')) 

        if answer == b'H':
            sr = guess
        if answer == b'S':
            er = guess
        if answer == b'G' or answer == b'L':
            finished = True

        time.sleep(0.25)
    
    s.close()
    if answer == b'G':
        print("I am the winner with", guess, "in", step_count, "steps")
    else:
        print("I lost !!!")


def main():
    try:
        s = socket.create_connection((host, port_tcp)) # connect to the server via TCP
    except socket.error as msg:
        print("Error: ", msg.strerror)
        exit(-1)

    data = s.recv(1024)
    print(data.decode('ascii'))

    run(s)

main()