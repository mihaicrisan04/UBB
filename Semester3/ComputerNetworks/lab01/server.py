import socket
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.bind(("0.0.0.0", 5555))
buff, addr = s.recvfrom(10)
print(buff.decode('utf-8'))
s.sendto(b"hello", addr)