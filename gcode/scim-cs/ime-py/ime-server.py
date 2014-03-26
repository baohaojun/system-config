#!/usr/bin/env python3
from socket import *
import threading
import os, sys, bhj_ime

def ime_handler(sock):
    sock = sock.makefile("rwb", 0)
    ime = bhj_ime.ime(sock, sock)
    ime.handle()

ime_listen_sock = socket(AF_INET, SOCK_STREAM);

if os.name == 'posix':
    ime_listen_sock.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)

ime_listen_sock.bind(('127.0.0.1', 31415))
ime_listen_sock.listen(5)

bhj_ime.init()

while True:
    ime_client_sock = ime_listen_sock.accept()[0]
    thread = threading.Thread(target=ime_handler, args=(ime_client_sock,))
    thread.start()
    del ime_client_sock

