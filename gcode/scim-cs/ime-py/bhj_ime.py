#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from __future__ import with_statement
import os, sys, re, traceback
from ime_ascii import *
from contextlib import contextmanager, closing
import threading, special_keys, pickle
from OrderedSet import OrderedSet
import subprocess

dynamic_freq = False

def debug(*args):
    print(*args)
    sys.stdout.flush()

def get_shell_ouput(args):
    """Invoke a pipe, and read the output back as a string"""
    p = subprocess.Popen(args, stdout = subprocess.PIPE)
    return p.stdout.read().decode('utf-8')

def shell_command(args):
    """Execute a shell command"""
    subprocess.call(args)

@contextmanager
def autolock(lock):
    lock.acquire()
    try:
        yield
    finally:
        lock.release()

class ime_trans:
    def __init__(self):
        self.lock = threading.RLock()
        with autolock(self.lock):
            import wubi86_trans
            self.rules = wubi86_trans.g_trans_map

    def has_trans(self, prefix):
        with autolock(self.lock):
            if prefix in self.rules:
                return True
            else:
                return False

    def build_trans(self, comp):
        with autolock(self.lock):
            for i in range(1, len(comp)):
                prefix = comp[0:i]
                if prefix not in self.rules:
                    self.rules[prefix] = comp[i]
                    continue
                if comp[i] not in self.rules[prefix]:
                    trans = list(self.rules[prefix])
                    trans.append(comp[i])
                    trans.sort()
                    self.rules[prefix] = ''.join(trans)


    def get_trans(self, prefix):
        with autolock(self.lock):
            if prefix in self.rules:
                return '[%s]' % self.rules[prefix]
            else:
                return ''

class ime_history:
    def __init__(self):
        self.lock = threading.RLock()
        assert _g_ime_quail, "quail must be inited before history"
        with autolock(self.lock):
            self.rules = {}

    def set_history(self, comp, idx):
        with autolock(self.lock):
            if comp not in self.rules:
                if idx != 0:
                    self.rules[comp] = OrderedSet()
                else:
                    return

            set_ = OrderedSet((idx,))
            self.rules[comp] = set_ | self.rules[comp]
            _g_ime_quail.adjust_history(comp, list(self.rules[comp]))
            del self.rules[comp]

    def get_history(self, comp):
        with autolock(self.lock):
            if comp in self.rules:
                return next(iter(self.rules[comp]))
            else:
                return 0

class ime_quail:
    def __init__(self):
        import wubi86
        assert _g_ime_trans, "trans must be inited before quail"
        self.lock = threading.RLock()
        self.rules = wubi86.g_quail_map
        self.__save_path = os.path.expanduser("~/.sdim/cands")
        os.system("mkdir -p ~/.sdim/cands")

        if dynamic_freq:
            for t in os.walk(self.__save_path):
                for x in t[2]:
                    f = os.path.join(t[0], x)
                    try:
                        comp = os.path.basename(f)
                        cands = pickle.load(open(f, "rb"))
                        if comp not in self.rules:
                            _g_ime_trans.build_trans(comp)
                            self.rules[comp] = cands
                    except:
                        exc_info = sys.exc_info()
                        traceback.print_stack()
                        sys.stderr.flush()


    def has_quail(self, comp):
        with autolock(self.lock):
            if comp in self.rules:
                return True
            else:
                return False

    def get_cands(self, comp):
        with autolock(self.lock):
            if self.has_quail(comp):
                return self.rules[comp]
            else:
                return ()

    def add_cand(self, comps, cand):
        with autolock(self.lock):
            for comp in comps:
                if self.has_quail(comp) and cand in self.rules[comp]:
                    continue
                if self.has_quail(comp):
                    self.rules[comp] = list(self.rules[comp])
                    self.rules[comp][:0] = [cand]
                else:
                    self.rules[comp] = (cand,)

                self.__save_comp(comp)

    def adjust_history(self, comp, history):
        if not dynamic_freq:
            return
        with autolock(self.lock):
            cands = OrderedSet()
            for h in history:
                cands.add(self.rules[comp][h])
            cands |= OrderedSet(self.rules[comp])
            self.rules[comp] = tuple(cands)
            self.__save_comp(comp)

    def __save_comp(self, comp):
        try:
            file_ = open(os.path.expanduser("~/.sdim/cands/" + comp), "wb")
            pickle.dump(self.rules[comp], file_)
        except:
            exc_info = sys.exc_info()
            traceback.print_stack()
            sys.stderr.flush()


class ime_reverse:
    def __init__(self):
        self.lock = threading.RLock()
        import wubi86_reverse
        self.rules = wubi86_reverse.g_reverse_map

    def has_reverse(self, han):
        with autolock(self.lock):
            return han in self.rules

    def get_reverse(self, han):
        with autolock(self.lock):
            if self.has_reverse(han):
                return self.rules[han]
            else:
                return ()

_g_ime_reverse = None
_g_ime_trans = None
_g_ime_quail = None
_g_ime_history = None

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
            assert isgraph(self.key), "key not graphic" #' ' is 'space', see below
        else:
            #assert self.key in self.special_keys, "key is not special function, like in emacs"
            pass

        if self.key == 'space':
            self.key = ' '


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

    def isdigit(self):
        return self.isgraph() and isdigit(self.name)
    def isprint(self):
        return self.isgraph() or self == 'space'

    def isalpha(self):
        return self.isgraph() and isalpha(self.name)

    def is_lc_alpha(self):
        if self.isgraph():
            return ord(self.name) in range(ord('a'), ord('z')+1)
        else:
            return False

class ime:
    comp_empty_wanted_keys = ('C g', 'C q', 'C +')
    page_size = 10 #max cands per page
    use_cand_as_comp = True
    def __init__(self, in_, out_):
        global _g_ime_s2t_mode
        self.s2t_mode = _g_ime_s2t_mode
        self.escape_mode = False
        self.__unicode_cands = []
        self.special_keys = special_keys.special_keys
        self.check_env();
        self.__on = False
        self.__in = in_
        self.__out = out_
        self.last_key = self.key = ''
        self.compstr = ''
        self.cand_index = 0
        self.commitstr = ''
        self.beepstr = ''

    def check_env(self, arg = None):
        if os.path.exists(os.path.expanduser("~/.sdim.rc")):
            file = open(os.path.expanduser("~/.sdim.rc"))
            while True:
                line = file.readline()
                if line == '':
                    break
                if re.match(r'^\s*use_cand_as_comp\s*=\s*true\s*$', line, re.IGNORECASE):
                    ime.use_cand_as_comp = True
                elif re.match(r'^\s*use_cand_as_comp\s*=\s*false\s*$', line, re.IGNORECASE):
                    ime.use_cand_as_comp = False
            file.close()

        if os.path.exists(os.path.expanduser("~/.sdim/use_cand_as_comp=false")):
            ime.use_cand_as_comp = False
        elif os.path.exists(os.path.expanduser("~/.sdim/use_cand_as_comp=true")):
            ime.use_cand_as_comp = True

    @property
    def beepstr(self):
        return self.__beepstr

    @beepstr.setter
    def beepstr(self, value):
        self.__beepstr = value

    @property
    def cand_index(self):
        return self.__cand_index

    @cand_index.setter
    def cand_index(self, value):
        self.__cand_index = value

        if _g_ime_quail.has_quail(self.compstr) or self.compstr[0:2] == "zu" and self.__cands:
            if _g_ime_quail.has_quail(self.compstr):
                num_cands = len(_g_ime_quail.get_cands(self.compstr))
            else:
                num_cands = len(self.__unicode_cands)

            if not self.cand_index < num_cands:
                self.cand_index = max(0, num_cands-1)
                self.beepstr = 'Y'

            if self.cand_index < 0:
                self.cand_index = 0
                self.beepstr = 'Y'

            min_cand = self.cand_index // ime.page_size * ime.page_size
            max_cand_p1 = min (min_cand + ime.page_size, num_cands)
            if _g_ime_quail.has_quail(self.compstr):
                self.__cands = _g_ime_quail.get_cands(self.compstr)[min_cand : max_cand_p1]
            else:
                self.__cands = self.__unicode_cands[min_cand : max_cand_p1]
        elif self.compstr[0:2] != "zu":
            self.__cands = ()

    @property
    def commitstr(self):
        return self.__commitstr

    @commitstr.setter
    def commitstr(self, value):
        assert isinstance(value, str), "commitstr must be set to be a string"
        if self.last_key == "=" and value == "》":
            self.__commitstr = "> "
        else:
            self.__commitstr = value
        self.last_key = self.key

    @property
    def compstr(self):
        return self.__compstr

    @compstr.setter
    def compstr(self, value):
        assert isinstance(value, str), "compstr must be set to be a string"
        self.__compstr = value
        self.cand_index = _g_ime_history.get_history(self.compstr)

        self.hintstr = _g_ime_trans.get_trans(self.compstr)

        if _g_ime_quail.has_quail(self.compstr) \
                and not _g_ime_trans.has_trans(self.compstr) \
                and len(self.__cands) == 1 \
                and (not os.path.exists(os.path.join(os.environ["HOME"], ".sdim-single")) or
                     self.compstr[-1] == ' '):
            self.__commit_cand()

    @property
    def hintstr(self):
        return self.__hintstr

    @hintstr.setter
    def hintstr(self, value):
        self.__hintstr = value

    def __write(self, str_):
        self.__out.write(bytes(str_, 'utf-8'))

    def __qa_end(self):
        self.__write('end:\n')

    def __error(self):
        exc_info = sys.exc_info()
        debug("%s: %s\n" % (repr(exc_info[0]), repr(exc_info[1])))
        self.__write("%s: %s\n" % (repr(exc_info[0]), repr(exc_info[1])))

    def __reply(self, reply):
        self.__write(reply)
        self.__write('\n')


    def handle(self):
        while True:
            line = self.__in.readline()
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
                arg = line[pos+1 : ].strip()

            if not line:
                return
            try:
                if func == "keyed":
                    self.keyed(arg)
                else:
                    eval('self.%s' % func)(arg)
            except:
                traceback.print_exc()
                self.__error()
                sys.stderr.flush()
            finally:
                self.__qa_end()

    def want(self, arg):
        assert arg, "want must take non empty arg"

        arg = arg[:-1] #get rid of the '?' at the end
        key = ime_keyboard(arg)

        if key == 'C \\':
            self.__reply('yes')
        elif not self.__on:
            self.__reply('no')
        elif self.compstr:
            self.__reply('yes')
        elif key.name in ime.comp_empty_wanted_keys:
            self.__reply('yes')
        elif key.isgraph():
            self.__reply('yes')
        else:
            self.__reply('no')

    def __keyed_when_no_comp(self, key):
        comp = key.name
        if not self.commitstr and key == '.' and '0' <= self.last_key and self.last_key <= '9':
            self.commitstr += key.name
        elif _g_ime_quail.has_quail(comp) or key.isalpha() or key == ';':
            self.compstr += key.name
        elif _g_ime_trans.has_trans(comp):
            self.compstr += key.name
        else:
            self.commitstr += key.name

    def keyed_when_comp(self, key):
        if self.compstr[-1] in '\'`~?_*^!(){}$:<>"' and key == 'S space':
            key = ime_keyboard('space')
        comp = self.compstr + key.name;
        if _g_ime_quail.has_quail(comp): #we have cand
            self.compstr += key.name
        elif _g_ime_trans.has_trans(comp): #we have hope to get cand
            self.compstr += key.name
        else:
            self.cand_impossible_after_key(key)

    def cand_impossible_after_key(self, key):
        if _g_ime_quail.has_quail(self.compstr):
            self.cand_possible_before_key(key)
        else:
            self.__english_mode(key)

    def is_selection_key(self, key):
        """Handle selection keys"""
        if key == 'C n' or key == 'C f' or key == 'down':
            self.cand_index += 1
            return True
        elif key == 'C p' or key == 'C b' or key == 'up':
            self.cand_index -= 1
            return True
        elif key == 'next':
            self.cand_index += ime.page_size
            return True
        elif key == 'prior':
            self.cand_index -= ime.page_size
            return True
        return False

    def cand_possible_before_key(self, key): #impossible after key
        global _g_ime_single_mode
        if key == 'return':
            self.__english_mode(key)
        elif key == 'space':
            self.__commit_cand()
        elif key == 'backspace':
            self.__english_mode(key)
        elif key.isdigit():
            self.cand_index = (int(key.name) + 9) % 10 + self.cand_index // 10 * 10;
            self.__commit_cand()
        elif key.isalpha() or key.isprint():
            if _g_ime_single_mode:
                self.__english_mode(key)
            else:
                self.__commit_cand()
                self.__keyed_when_no_comp(key)
        elif self.is_selection_key(key):
            pass
        else:
            self.beepstr = 'y'

    def dump_comp(self, comp):
            if _g_ime_quail.has_quail(comp):
                debug("rules[" + comp + "]: " + repr(_g_ime_quail.get_cands(comp)))

    def add_cand(self, cand):
        if len(cand) < 2:
            debug("len(cand) must > 2")
            return
        comps = []
        if len(cand) == 2:
            code0 = _g_ime_reverse.get_reverse(cand[0])
            code1 = _g_ime_reverse.get_reverse(cand[1])
            if code0 and code1:
                for c0 in code0:
                    for c1 in code1:
                        comps.append(c0 + c1)
                _g_ime_quail.add_cand(comps, cand)
        if len(cand) == 3:
            code0 = _g_ime_reverse.get_reverse(cand[0])
            code1 = _g_ime_reverse.get_reverse(cand[1])
            code2 = _g_ime_reverse.get_reverse(cand[2])
            if code0 and code1 and code2:
                for c0 in code0:
                    for c1 in code1:
                        for c2 in code2:
                            comps.append(c0[0] + c1[0] + c2)
                _g_ime_quail.add_cand(comps, cand)
        if len(cand) > 3:
            code0 = _g_ime_reverse.get_reverse(cand[0])
            code1 = _g_ime_reverse.get_reverse(cand[1])
            code2 = _g_ime_reverse.get_reverse(cand[2])
            code3 = _g_ime_reverse.get_reverse(cand[-1])
            if code0 and code1 and code2 and code3:
                for c0 in code0:
                    for c1 in code1:
                        for c2 in code2:
                            for c3 in code3:
                                comps.append(c0[0] + c1[0] + c2[0] + c3[0])
                _g_ime_quail.add_cand(comps, cand)

    def keyed(self, arg):
        debug('keyed args:', arg)
        escape_mode = self.escape_mode
        self.key = arg
        key = ime_keyboard(arg)
        if key == 'escape':
            self.escape_mode = True
        else:
            self.escape_mode = False

        if key == 'C \\':
            self.__toggle()
        elif key == 'C y':
            self.compstr += get_shell_ouput(["getclip"])
        elif key == 'C g' or key == 'C /':
            self.compstr = ''
        elif key == 'C v':
            key = ime_keyboard('next')
            self.keyed_when_comp(key)
        elif key == 'escape' and escape_mode == True:
            self.escape_mode = False
            self.compstr = ''
        elif escape_mode == True:
            if key == 'v':
                key = ime_keyboard('prior')
                self.keyed_when_comp(key)
            elif key == 'backspace':
                last_word_re = re.compile(r'\b\w+\W*$')
                match = last_word_re.search(self.compstr)
                if match:
                    self.compstr = self.compstr[0:match.start()]
                else:
                    self.compstr = ''
            else:
                pass
        elif not self.compstr:
            self.__keyed_when_no_comp(key)
        elif self.compstr[0:4] == '!add':
            self.__add_word(key)
        else:
            self.keyed_when_comp(key)

        self.reply_comp()
        self.reply_hint()
        self.reply_cands()
        self.reply_active()
        self.reply_cand_idx()
        self.reply_commit()
        self.reply_beep()



    def is_commit_key(self, key):
        """Should we commit?"""
        if self.compstr[0:2] == "zu" and self.__cands:
            if key == 'space' or key.isdigit():
                if key.isdigit():
                    self.cand_index = (int(key.name) + 9) % 10 + self.cand_index // 10 * 10;
                commitstr = self.__cands[self.cand_index % ime.page_size]
                commitstr = commitstr.split(" ")[0]
                shell_command(["ime-commit-unicode", commitstr])
                self.compstr = ''
                self.__cands = []
                self.commitstr = commitstr
                return True
            elif key == 'C return':
                commitstr = "%0a".join(self.__unicode_cands)
                self.compstr = ''
                self.__cands = []
                self.commitstr = commitstr
                return True

    def __english_mode(self, key):
        if self.__cands:
            if self.is_selection_key(key):
                return

        if self.compstr[0:2] == "zu":
            if self.is_commit_key(key):
                return
            if (key == '.' or key != 'space'):
                if key == '.' and self.compstr[-1] != '.':
                    self.compstr += '.'
                elif key.isgraph():
                    self.compstr += key.name
                elif key == 'backspace':
                    self.compstr = self.compstr[0:-1]
                if len(self.compstr) > 4:
                    args = self.compstr[2:].split('.')
                    out = get_shell_ouput(["get-unicode-char"] + args).split("\n")
                    out = [x for x in out if x]
                    self.__unicode_cands = out
                    self.__cands = self.__unicode_cands[0 : 10]
                return

        if self.compstr == '; ' and key == 'space':
            self.commitstr += '；'
            self.compstr = ''
        elif self.compstr[0:3] == "z0x" and \
             re.match("^z0x[0-9a-f]+$", self.compstr, re.IGNORECASE) and \
             key == 'space':
            try:
                hex = self.compstr[1:]
                self.commitstr = chr(int(hex, 16))
                self.compstr = ''
            except:
                pass
        elif key.isprint():
            self.compstr += key.name
        elif key == 'backspace':
            self.compstr = self.compstr[:-1]
        elif key == 'return':
            comp = self.compstr
            if self.compstr and self.compstr[0] == ';' and len(self.compstr) > 1:
                comp = self.compstr[1:]
            self.commitstr += comp
            self.compstr = ''

    def __commit_cand(self):
        cand_index = self.cand_index % ime.page_size
        self.commitstr = self.__cands[cand_index]
        _g_ime_history.set_history(self.compstr, self.cand_index)
        self.compstr = ''

    def reply_beep(self):
        if self.beepstr:
            self.__reply('beep: %s' % self.beepstr)
            self.beepstr = ''

    def reply_commit(self):
        if self.commitstr == '!s':
            self.s2t_mode = False
        elif self.commitstr == '!t':
            self.s2t_mode = 't'
        elif self.commitstr == '!j':
            self.s2t_mode = 'j'
        elif self.commitstr:
            global _g_opencc_s2t
            global _g_opencc_s2j
            if self.s2t_mode == 't' and _g_opencc_s2t:
                self.__reply('commit: %s' % _g_opencc_s2t.convert(self.commitstr))
            elif self.s2t_mode == 'j' and _g_opencc_s2j:
                self.__reply('commit: %s' % _g_opencc_s2j.convert(self.commitstr))
            else:
                self.__reply('commit: %s' % self.commitstr)

        self.commitstr = ''

    def reply_hint(self, arg=''):
        if self.hintstr:
            self.__reply('hint: ' + self.hintstr)

    def reply_comp(self, arg=''):
        if self.__cands and ime.use_cand_as_comp:
            comp = ''
            try:
                if self.compstr[0:2] == 'zu':
                    comp = self.compstr
                else:
                    bytes(self.__cands[self.cand_index % ime.page_size], 'utf-8')
                    comp = self.__cands[self.cand_index % ime.page_size]
            except:
                comp = '?'
            self.__reply('comp: ' + comp)
            return
        if self.compstr:
            self.__reply('comp: ' + self.compstr)

    def reply_cands(self, arg=''):
        cands = []
        for x in self.__cands:
            x = x.replace('%', '%25')
            x = x.replace(' ', '%20')
            try:
                bytes(x, 'utf-8')
                cands.append(x)
            except:
                cands.append('?')
        if cands:
            self.__reply('cands: ' + ' '.join(cands))

    def reply_active(self, arg=''):
        if self.__on:
            self.__reply('active: Y')
        else:
            self.__reply('active: N')

    def reply_cand_idx(self, arg=''):
        if self.cand_index:
            self.__reply('cand_index: %d' % self.cand_index)

    def __toggle(self):
        self.__on = not self.__on

        if not self.__on:
            self.compstr = ''

def init():
    global _g_ime_reverse
    _g_ime_reverse = ime_reverse()

    global _g_ime_single_mode
    _g_ime_single_mode = os.path.exists(os.path.join(os.environ["HOME"], ".sdim-single"))

    global _g_ime_s2t_mode
    _g_ime_s2t_mode = os.path.exists(os.path.join(os.environ["HOME"], ".sdim-s2t"))
    global _g_opencc_s2t
    global _g_opencc_s2j
    try:
        import opencc
        _g_opencc_s2t = opencc.OpenCC("s2t.json")
        _g_opencc_s2j = opencc.OpenCC("s2j.json")
    except:
        pass

    global _g_ime_trans
    _g_ime_trans = ime_trans()

    global _g_ime_quail
    _g_ime_quail = ime_quail()

    global _g_ime_history
    _g_ime_history = ime_history()
    print('ime init complete')
    sys.stdout.flush()
if __name__ == '__main__':
    init()
    ime_ = ime(open("/dev/stdin", "rb", 0), open("/dev/stdout", "wb", 0))
    ime_.handle()
