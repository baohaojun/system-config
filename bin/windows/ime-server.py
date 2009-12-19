#!/bin/python
from socket import *
from thread import *
import os, sys, re

class ime:
    def __init__(self):
        self.on = False
        


def ime_handler(sock):
    sock = sock.makefile("rw", 0)
    while True:
        line = sock.readline()
        if not line:
            return
        while line and line[-1] in ('\r', '\n'):
            line = line[:-1]

        print 'received', line

        if line == "exit":
            print "client want to exit"
            return
        
        global g_quail_rules
        if line in g_quail_rules:
            for x in g_quail_rules[line]:
                sock.write(x+"\n");
            sock.write('end:\n')
        else:
            sock.write('no such key!\n')
            sock.write('end:\n')


ime_listen_sock = socket(AF_INET, SOCK_STREAM);
ime_listen_sock.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
ime_listen_sock.bind(('0.0.0.0', 12345))
ime_listen_sock.listen(5)

g_ime_file = open(os.path.join(os.environ['HOME'], '.emacs_d', 'lisp', 'quail', 'wubi86.el'),
                  'r')

g_quail_rules = {}

def init_quail():

    start = False
    reobj = re.compile('^\("(.*?)" \[(.*)\]\)$')

    def init_one_rule(line):
        global g_quail_rules
        mo = reobj.match(line)
        if mo:
            key = mo.group(1)
            cands = mo.group(2).split('" "')

            cands[0] = cands[0][1:]
            cands[-1] = cands[-1][:-1]
            g_quail_rules[key] = cands

    while True:
        line = g_ime_file.readline()
        if not line:
            break
        if line == '(quail-define-rules\n':
            start = True 
            
        if not start:
            continue

        init_one_rule(line)

init_quail()
print 'quail init complete'


while True:
    ime_client_sock = ime_listen_sock.accept()[0]
    start_new_thread(ime_handler, (ime_client_sock,))
    del ime_client_sock

