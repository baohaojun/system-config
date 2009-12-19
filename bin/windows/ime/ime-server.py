#!/bin/python
from socket import *
from thread import *
import os, sys, bhj_ime

def ime_handler(sock):
    sock = sock.makefile("rw", 0)
    ime = bhj_ime.ime(sock)
    ime.handle()

ime_listen_sock = socket(AF_INET, SOCK_STREAM);
ime_listen_sock.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
ime_listen_sock.bind(('0.0.0.0', 12345))
ime_listen_sock.listen(5)

print 'quail init begin'
bhj_ime.init()
print 'quail init complete'


while True:
    ime_client_sock = ime_listen_sock.accept()[0]
    start_new_thread(ime_handler, (ime_client_sock,))
    del ime_client_sock

