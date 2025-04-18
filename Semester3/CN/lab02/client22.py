import socket

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

s.connect(("127.0.0.1", 5555))

words = ["hello", "world", "from", "client"]
message = ",".join(words).encode('utf-8')

s.sendall(message)

buff = s.recv(20)
print(f"Received from server: {buff.decode('utf-8')}")

s.close()