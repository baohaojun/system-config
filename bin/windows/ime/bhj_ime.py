#!/bin/env python
from __future__ import with_statement
import os, sys, re, traceback
from ime_ascii import *
from contextlib import contextmanager, closing
import threading, special_keys
from OrderedSet import OrderedSet



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

class ime_reverse:
    def __init__(self):
        self.rules = {}
        self.lock = threading.RLock()
        
    def has_han(self, han):
        with autolock(self.lock):
            return han in self.rules

    def set_reverse(self, han, codes):
        with autolock(self.lock):
            self.rules[han] = codes

    def get_reverse(self, han):
        with autolock(self.lock):
            if self.has_han(han):
                return self.rules[han]
            else:
                return ()

_g_ime_reverse = ime_reverse()
_g_ime_trans = ime_trans()
_g_ime_quail = ime_quail()

class ime_keyboard:
    all_mods = ['', #no modifier
                  'A', # alt
                  'AC',# alt ctrl
                  'ACS', #alt ctrl shift
                  'AS',
                  'C',
                  'CS',
                  'S'
                  ]

    def __init__(self, keystr):
        self.special_keys = special_keys.special_keys
        keys = keystr.split()
        mods = keys[:-1]
        mods = ''.join(mods)
        mods = list(mods)
        mods.sort()
        mods = OrderedSet(mods)
        self.mods = ''.join(mods)
        
        assert self.mods in ime_keyboard.all_mods, "invalid modifiers"
        
        self.key = keys[-1]
        
        assert len(self.key), "empty keys"
        if len(self.key) == 1:
            assert isgraph(self.key), "key not graphic"
        else:
            assert self.key in self.special_keys, "key is not special function, like in emacs"

    def __eq__(self, other_key):
        if isinstance(other_key, str):
            other_key = ime_keyboard(other_key)
        return self.name == other_key.name

    @property
    def name(self):
        if not self.mods:
            return self.key
        else:
            return self.mods + ' ' + self.key

class ime:
    def __init__(self, sock):
        self.special_keys = special_keys.special_keys
        self.__on = False
        self.__sock = sock
        self.__compstr = ''
        self.__hintstr = ''
        self.__cands = []
        self.__cand_index = 0

    def __write(self, str_):
        self.__sock.write(bytes(str_, 'utf-8'))

    def __qa_end(self):
        self.__write('end:\n')

    def __error(self):
        exc_info = sys.exc_info()
        traceback.print_tb(exc_info[2])
        sys.stderr.flush()
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
        key = ime_keyboard(arg)

        if key == 'C \\':
            self.__toggle()
        elif len(key.name) == 1 and ord(key.name) in range(ord('a'), ord('z')+1):
            self.__compstr += key.name
        
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
        mo = reobj.match(line)
        if mo:
            key = mo.group(1)
            cands = mo.group(2).split('" "')
            cands[0] = cands[0][1:]
            cands[-1] = cands[-1][:-1]
            _g_ime_reverse.set_reverse(key, tuple(cands))

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
                                   '2.el'),
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
