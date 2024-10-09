import socket

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

s.connect(("127.0.0.1", 5555))

s.sendall(b"Hello from client!")

buff = s.recv(20)
print(f"Received: {buff.decode('utf-8')}")

s.close()