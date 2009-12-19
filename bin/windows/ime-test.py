#!/bin/python
from socket import *
from thread import *


sock = socket(AF_INET, SOCK_STREAM);
sock.connect(('127.0.0.1', 12345))

sock = sock.makefile("rw", 0)

while True:
    global sock
    line = stdin.readline()
    if not line:
        break
    
    sock.write(line)
    line = sock.readline()
    print line,
