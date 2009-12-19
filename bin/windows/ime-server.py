#!/bin/env pystart
from socket import *
from thread import *

ime_listen_sock = socket(AF_INET, SOCK_STREAM);
ime_listen_sock.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
ime_listen_sock.bind(('127.0.0.1', 12345))
ime_listen_sock.listen(5)


while True:
    ime_client_sock = ime_listen_sock.accept()[0]
    start_new_thread(ime_handler, (ime_client_sock,))
    close(ime_client_sock)

def ime_handler(sock):
    sock = sock.makefile("rw", 0)
    while True:
        line = sock.readline()
        if not line:
            return
        line = line[:-1]

        if line == "exit":
            return
        
        sock.write(line+"\n");

