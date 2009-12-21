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
        
    def isgraph(self):
        if not self.mods and len(self.key) == 1:
            return True
        else:
            return False

    def isprint(self):
        return self.isgraph() or self == 'space'

    def is_lc_alpha(self):
        if self.isgraph():
            return ord(self.name) in range(ord('a'), ord('z')+1)
        else:
            return False

class ime:
    comp_empty_wanted_keys = ('C g', 'C q', 'C +')
    mcpp = 10 #max cands per page
    def __init__(self, sock):
        self.special_keys = special_keys.special_keys
        self.__on = False
        self.__sock = sock
        self.__compstr = ''
        self.__hintstr = ''
        self.__cands = []
        self.__cand_index = 0
        self.__commitstr = ''

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
        key = ime_keyboard(arg)

        if key == 'C \\':
            self.__reply('yes')
        elif not self.__on:
            self.__reply('on')
        elif self.__compstr:
            self.__reply('yes')
        elif key.name in ime.comp_empty_wanted_keys:
            self.__reply('yes')
        elif key.isgraph():
            self.__reply('yes')

        self.__reply('no')

    def __return(self):
        self.__commitstr += self.__compstr
        self.__cancel_ime()

    def __keyed_when_no_comp(self, key):
        if key.isalpha() or key == ';':
            self.__commitstr += key.name
        else:
            self.__commit(key.name)

    def __commit(self, commitstr):
        self.commitstr += commitstr

    def __cancel_ime(self):
        self.__compstr = ''
        self.__cands = []
        self.__cand_index = 0
        self.__hintstr = ''

    def __space(self):
        pass

    def __backspace(self):
        pass

    def __digit(self, key):
        pass

    def __lc_alpha(self, key):
        pass
    def keyed(self, arg):
        key = ime_keyboard(arg)

        if key == 'C \\':
            self.__toggle()
        elif key == 'C g':
            self.__cancel_ime()
        elif not self.__compstr:
            self.__keyed_when_no_comp(key)
        elif self.__compstr[0:4] == '!add':
            self.__add_word(key)
        elif self.__compstr[0] == ';':
            self.__english_mode(key)
        elif key == 'return':
            self.__return()
        elif key == 'space':
            self.__space()
        elif key == 'backspace':
            self.__backspace()
        elif key.isdigit():
            self.__digit(key.name)
        elif key.is_lc_alpha():
            self.__lc_alpha(key.name)
        elif key == 'C n':
            self.__next_page_cand()
        elif key == 'C p':
            self.__prev_page_cand()
        elif key == 'C f':
            self.__next_cand()
        elif key == 'C b':
            self.__prev_cand()
        elif key.isprint():
            if self.__cands:
                self.__commit_cand()
            return self.keyed(key.name)

        else:
            self.__beep()
            
        self.comp()
        self.hint()
        self.cands()
        self.active()
        self.cand_index()

    def __next_page_cand(self):
        pass
    def __prev_page_cand(self):
        pass

    def __next_cand(self):
        pass
    def __prev_cand(self):
        pass

    def __commit_cand(self):
        pass

    def __beep(self):
        pass

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
