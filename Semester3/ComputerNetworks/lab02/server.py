import socket

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

s.bind(("0.0.0.0", 5555))
s.listen(1)

conn, addr = s.accept()

buff = conn.recv(20)
print(buff.decode('utf-8'))

conn.sendall(b"hello from server")

conn.close()