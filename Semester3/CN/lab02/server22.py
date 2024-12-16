import socket

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

s.bind(("0.0.0.0", 5555))
s.listen(1)

conn, addr = s.accept()

# recv(n) - receive n bytes
buff = conn.recv(100).decode('utf-8')

words = buff.split(',')
count = len(words)

print(words)
response = f"{count}".encode('utf-8')

conn.sendall(response)

conn.close()