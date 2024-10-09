import socket

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

s.bind(("0.0.0.0", 5555))
s.listen(1)

conn, addr = s.accept()

buff = conn.recv(20).decode('utf-8')

num1, num2 = buff.split(',')
num1 = int(num1)
num2 = int(num2)
sum = num1 + num2

print(f"Received from client: {num1} and {num2}")
response = f"{sum}".encode('utf-8')

conn.sendall(response)

conn.close()