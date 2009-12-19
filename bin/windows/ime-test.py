#!/bin/python
from socket import *
from thread import *
import sys

sock = socket(AF_INET, SOCK_STREAM);
sock.connect(('127.0.0.1', 12345))

sock = sock.makefile("rw", 0)

while True:
    line = sys.stdin.readline()
    if not line:
        break
    
    sock.write(line)
    while True:
        line = sock.readline()
        if not line or line == 'end:\n':
            break
        print line,

