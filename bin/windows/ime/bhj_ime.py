#!/bin/env python
from __future__ import with_statement
import os, sys, re, traceback
from ime_ascii import *
from contextlib import contextmanager, closing
import threading, special_keys
from OrderedSet import OrderedSet


_g_reverse_rules = {}


@contextmanager
def autolock(lock):
    lock.acquire()
    try:
        yield
    finally:
        lock.release()

class ime_trans:
    def __init__(self):
        self.rules = {}
        self.lock = threading.RLock()
        
    def has_prefix(self, prefix):
        with autolock(self.lock):
            if prefix in self.rules:
                return True
            else:
                return False

    def add_trans(self, key):
        with autolock(self.lock):
            for i in range(1, len(key)):
                prefix = key[:i]
                if not self.has_prefix(prefix):
                    self.rules[prefix] = set()

                self.rules[prefix].add(key[i])

class ime_quail:
    def __init__(self):
        self.rules = {}
        self.lock = threading.RLock()

    def has_comp(self, comp):
        with autolock(self.lock):
            if comp in self.rules:
                return True
            else:
                return False

    def add_comp_cand(self, comp, cand):
        with autolock(self.lock):
            if not self.has_comp(comp):
                self.rules[comp] = OrderedSet()
            
            self.rules[comp].add(cand)

    def add_comp_ncands(self, comp, cands):
        with autolock(self.lock):
            if not self.has_comp(comp):
                self.rules[comp] = OrderedSet(cands)
            else:
                for cand in cands:
                    self.add_comp_cand(comp, cand)

    def get_cands(self, comp):
        with autolock(self.lock):
            if self.has_comp(comp):
                return self.rules[comp]
            else:
                return OrderedSet()


_g_ime_trans = ime_trans()
_g_ime_quail = ime_quail()

class ime_keyboard:
    def __init__(self):
        self.special_keys = special_keys.special_keys

class ime:
    def __init__(self, sock):
        self.special_keys = special_keys.special_keys
        self.__on = False
        self.__sock = sock
        self.__compstr = ''
        self.__hintstr = ''
        self.__cands = []
        self.__cand_index = 0
        self.__all_mods = ['', #no modifier
                           'A', # alt
                           'AC',# alt ctrl
                           'ACS', #alt ctrl shift
                           'AS',
                           'C',
                           'CS',
                           'S'
                           ]

    def __write(self, str_):
        self.__sock.write(bytes(str_, 'utf-8'))

    def __qa_end(self):
        self.__write('end:\n')

    def __error(self):
        exc_info = sys.exc_info()
        self.__write("%s: %s\n" % (repr(exc_info[0]), repr(exc_info[1])))

    def __reply(self, reply):
        self.__write(reply)
        self.__write('\n')


    def handle(self):
        while True:
            line = self.__sock.readline()
            if not line:
                return
            line = line.decode('utf-8')
            while line and line[-1] in ('\r', '\n'):
                line = line[:-1]
            
            pos = line.find(' ')
            if pos == -1:
                func = line
                arg = ''
            else :
                func = line[:pos]
                arg = line[pos+1 : ]
            try:
                eval('self.%s' % func)(arg)
            except:
                self.__error()
            finally:
                self.__qa_end()

    def want(self, arg):
        assert arg, "want must take non empty arg"
        
        arg = arg[:-1] #get rid of the '?' at the end
        keys = arg.split()

        assert keys, "want must take at least 1 key"

        mods = keys[:-1]
        mods.sort()
        mods = ''.join(mods)
        assert mods in self.__all_mods, "invalid modifiers"
        key = keys[-1]

        if mods == 'C' and key == '\\':
            self.__reply('yes')
        else:
            self.__reply('no')

    def keyed(self, arg):
        keys = arg.split()
        assert keys, "empty keys"

        mods = keys[:-1]
        mods.sort()
        mods = ''.join(mods)
        
        assert mods in self.__all_mods, "invalid modifiers"
        
        key = keys[-1]
        
        assert len(key), "empty keys"
        if len(key) == 1:
            assert isgraph(key), "key not graphic"
        else:
            assert key in self.special_keys, "key is not special function, like in emacs"

        if mods == 'C' and key == '\\':
            self.__toggle()
        elif not mods and len(key) == 1 and ord(key) in range(ord('a'), ord('z')+1):
            self.__compstr += key
        
        self.comp()
        self.hint()
        self.cands()
        self.active()
        self.cand_index()


    def hint(self, arg=''):
        self.__reply('hint:')
        self.__reply(self.__hintstr)
        self.__reply('hint end:')

    def comp(self, arg=''):
        self.__reply('comp:')
        self.__reply(self.__compstr)
        self.__reply('comp end:')
            
    def cands(self, arg=''):
        self.__reply('cands:')
        for x in _g_ime_quail.get_cands(self.__compstr):
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


def _init_reverse():
    reobj = re.compile('^"(.*?)" (.*)$')


    def init_one_rule(line):
        global _g_reverse_rules
        mo = reobj.match(line)
        if mo:
            key = mo.group(1)
            cands = mo.group(2).split('" "')
            cands[0] = cands[0][1:]
            cands[-1] = cands[-1][:-1]
            _g_reverse_rules[key] = cands

    with closing(open(os.path.join(os.environ['HOME'], 
                                   '.emacs_d', 
                                   'lisp', 
                                   'quail', 
                                   'reverse.txt'),
                      'r', encoding='utf-8')) as reverse_file:
        for line in reverse_file:
            init_one_rule(line)


def _init_quail():
    start = False
    reobj = re.compile('^\("(.*?)" \[(.*)\]\)$')

    def init_one_trans(key):
        _g_ime_trans.add_trans(key)

    def init_one_rule(line):
        mo = reobj.match(line)
        if mo:
            key = mo.group(1)
            init_one_trans(key)

            cands = mo.group(2).split('" "')

            cands[0] = cands[0][1:]
            cands[-1] = cands[-1][:-1]

            _g_ime_quail.add_comp_ncands(key, cands)

    with closing(open(os.path.join(os.environ['HOME'], 
                                   '.emacs_d', 
                                   'lisp', 
                                   'quail', 
                                   'wubi86.el'),
                      'r', encoding = 'utf-8'
                      )
                 ) as quail_file:
        for line in quail_file:
            if line == '(quail-define-rules\n':
                start = True 

            if not start:
                continue

            init_one_rule(line)

def init():
    _init_quail()
    _init_reverse()
    print('ime init complete')
    sys.stdout.flush()
if __name__ == '__main__':
    init()
