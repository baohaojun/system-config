#!/bin/python
import os, sys, re
_g_ime_file = open(os.path.join(os.environ['HOME'], '.emacs_d', 'lisp', 'quail', 'wubi86.el'),
                  'r')

_g_quail_rules = {}

class ime:
    def __init__(self, sock):
        self.__on = False
        self.__sock = sock
        self.__compstr = ''
        self.__cands = []
        self.__cand_index = 0

    def __qa_end(self):
        self.__sock.write('end:\n')

    def __error(self, err):
        self.__sock.write("%s\n" % err)

    def __reply(self, reply):
        self.__sock.write(reply)
        self.__sock.write('\n')


    def handle(self):
        while True:
            line = self.__sock.readline()
            if not line:
                return
            while line and line[-1] in ('\r', '\n'):
                line = line[:-1]

            pos = line.find(' ')
            if pos == -1:
                func = line
                arg = ''
            else :
                func = line[:pos]
                arg = line[pos+1 : ]

            print "call `%s' with `%s'" % (func, arg)

            try:
                apply(eval('self.%s' % func), (arg,))
            finally:
                self.__qa_end()

    def want(self, arg):
        if not arg:
            return self.__error("Error: I want something!")
        
        arg = arg[:-1] #get rid of the '?' at the end
        keys = arg.split()

        if not keys:
            return self.__error("Error: I want something!")

        mods = keys[:-1]
        mods.sort()
        mods = ''.join(mods)
        key = keys[-1]

        if mods == 'C' and key == '\\':
            self.__reply('yes')
        else:
            self.__reply('no')

    def keyed(self, arg):
        keys = arg.split()
        if not keys:
            return self.__error("Error: keyed nothing?")

        mods = keys[:-1]
        mods.sort()
        mods = ''.join(mods)
        
        key = keys[-1]

        if mods == 'C' and key == '\\':
            self.__toggle()
        elif not mods and len(key) == 1 and ord(key) in range(ord('a'), ord('z')+1):
            self.__compstr += key
        
        self.comp()
        self.cands()
        self.active()
        self.cand_index()

    def comp(self, arg=''):
        self.__reply('comp:')
        self.__reply(self.__compstr)
        self.__reply('comp end:')
            
    def cands(self, arg=''):
        self.__reply('cands:')
        for x in self.__cands:
            self.__reply(x)     
        self.__reply('cands end:')

    def active(self, arg=''):
        self.__reply('active:')
        if self.__on:
            self.__reply('yes')
        else:
            self.__reply('no')
        self.__reply('active end:')

    def cand_index(self, arg=''):
        self.__reply('cand_index:')
        self.__reply('%d' % self.__cand_index)
        self.__reply('cand_index end:')

    def __toggle(self):
        self.__on = not self.__on

        if not self.__on:
            self.__compstr = ''
            self.__cands = []




def init():
    start = False
    reobj = re.compile('^\("(.*?)" \[(.*)\]\)$')

    def init_one_rule(line):
        global _g_quail_rules
        mo = reobj.match(line)
        if mo:
            key = mo.group(1)
            cands = mo.group(2).split('" "')

            cands[0] = cands[0][1:]
            cands[-1] = cands[-1][:-1]
            _g_quail_rules[key] = cands

    while True:
        line = _g_ime_file.readline()
        if not line:
            break
        if line == '(quail-define-rules\n':
            start = True 
            
        if not start:
            continue

        init_one_rule(line)

if __name__ == '__main__':
    print 'quail init begin'
    init()
    print 'quail init complete'
