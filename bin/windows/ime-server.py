#!/bin/python
from socket import *
from thread import *


def ime_handler(sock):
    sock = sock.makefile("rw", 0)
    while True:
        line = sock.readline()
        if not line:
            return
        while line[-1] in ('\r', '\n'):
            line = line[:-1]

        print 'received ', 
        for x in line: print "%02x " % ord(x),
        print
        if line == "exit":
            return
        
        sock.write(line+"\n");


ime_listen_sock = socket(AF_INET, SOCK_STREAM);
ime_listen_sock.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
ime_listen_sock.bind(('0.0.0.0', 12345))
ime_listen_sock.listen(5)


while True:
    ime_client_sock = ime_listen_sock.accept()[0]
    start_new_thread(ime_handler, (ime_client_sock,))
    del ime_client_sock

