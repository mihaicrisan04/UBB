import socket

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

s.connect(("127.0.0.1", 5555))

# Define two numbers
num1 = 42
num2 = 17

# Convert the numbers to a string and encode to bytes
message = f"{num1},{num2}".encode('utf-8')

s.sendall(message)

buff = s.recv(20)
print(f"Received from server: {buff.decode('utf-8')}")

s.close()