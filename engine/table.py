# -*- coding: utf-8 -*-
# vim:et sts=4 sw=4
#
# ibus-table - The Tables engine for IBus
#
# Copyright (c) 2008-2009 Yu Yuwei <acevery@gmail.com>
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
# $Id: $
#
__all__ = (
    "tabengine",
)

import os
import ibus
#from ibus import Property
from ibus import keysyms
from ibus import modifier
from ibus import ascii
#import tabsqlitedb
import tabdict
import re
patt_edit = re.compile (r'(.*)###(.*)###(.*)')
patt_uncommit = re.compile (r'(.*)@@@(.*)')

from gettext import dgettext
_  = lambda a : dgettext ("ibus-table", a)
N_ = lambda a : a

import dbus
from socket import *

class KeyEvent:
    all_mods = [
        '', #no modifier
        'A', # alt
        'AC',# alt ctrl
        'ACS', #alt ctrl shift
        'AS',
        'C',
        'CS',
        'S'
        ]
    def __init__(self, keyval, is_press, state):
        self.code = keyval
        self.mask = state
        self.name = ''
        if not is_press:
            self.mask |= modifier.RELEASE_MASK
            return
        
        try:
            self.name = chr(self.code)
            if self.name == ' ':
                self.name = 'space'
            else:
                self.mask &= ~modifier.SHIFT_MASK
        except:
            self.name = keysyms.keycode_to_name(self.code).lower()
        
        if self.name in ("control_l",
                         "control_r",
                         "alt_l",
                         "alt_r",
                         "shift_l",
                         "shift_r",
                         ):
            self.name = ""
            return

        mods = ''
        if self.mask & modifier.ALT_MASK:
            mods += 'A'
        if self.mask & modifier.CONTROL_MASK:
            mods += 'C'
        if self.mask & modifier.SHIFT_MASK:
            mods += 'S'

        if mods != '':
            self.name = mods + ' ' + self.name
    def __str__(self):
        return self.name


class editor(object):
    '''Hold user inputs chars and preedit string'''
    def __init__ (self, config, phrase_table_index,valid_input_chars, max_key_length, database, parser = tabdict.parse, deparser = tabdict.deparse, max_length = 64):
        self.db = database
        self._config = config
        self._name = self.db.get_ime_property('name')
        self._config_section = "engine/Table/%s" % self._name
        self._pt = phrase_table_index
        self._parser = parser
        self._deparser = deparser
        self._max_key_len = int(max_key_length)
        self._max_length = max_length
        self._valid_input_chars = valid_input_chars
        #
        # below vals will be reset in self.clear()
        #
        # we hold this: [str,str,...]
        # self._chars: hold user input in table mode (valid,invalid,prevalid)
        self._chars = [[],[],[]]
        #self._t_chars: hold total input for table mode for input check
        self._t_chars = []
        # self._u_chars: hold user input but not manual comitted chars
        self._u_chars = []
        # self._tabkey_list: hold tab_key objects transform from user input chars
        self._tabkey_list = []
        # self._strings: hold preedit strings
        self._strings = []
        # self._cursor: the caret position in preedit phrases 
        self._cursor = [0,0]
        # self._candidates: hold candidates selected from database [[now],[pre]]
        self._candidates = [[],[]]
        self._lookup_table = ibus.LookupTable (tabengine._page_size)
        # self._py_mode: whether in pinyin mode
        self._py_mode = False
        # self._zi: the last Zi commit to preedit
        self._zi = u''
        # self._caret: caret position in lookup_table
        self._caret = 0
        # self._onechar: whether we only select single character
        self._onechar = self._config.get_value (self._config_section, "OneChar", False)
        # self._chinese_mode: the candidate filter mode,
        #   0 is simplify Chinese
        #   1 is traditional Chinese
        #   2 is Big charset mode, but simplify Chinese first
        #   3 is Big charset mode, but traditional Chinese first
        #   4 is Big charset mode.
        # we use LC_CTYPE or LANG to determine which one to use
        self._chinese_mode = self._config.get_value (
                self._config_section,
                "ChineseMode",
                self.get_chinese_mode())

    def get_chinese_mode (self):
        '''Use LC_CTYPE in your box to determine the _chinese_mode'''
        try:
            if os.environ.has_key('LC_CTYPE'):
                __lc = os.environ['LC_CTYPE'].split('.')[0].lower()
            else:
                __lc = os.environ['LANG'].split('.')[0].lower()

            if __lc.find('zh_') == 0:
                # this is a zh_XX
                __place =__lc.split('_')[1]
                if __place == 'cn':
                    return 0
                else:
                    return 1
            else:
                if self.db._is_chinese:
                    # if IME declare as Chinese IME
                    return 0
                else:
                    return -1
        except:
            return -1
                
    def change_chinese_mode (self):
        if self._chinese_mode != -1:
            self._chinese_mode = (self._chinese_mode +1 ) % 5
        self._config.set_value (
                self._config_section,
                "ChineseMode",
                self._chinese_mode )

    def clear (self):
        '''Remove data holded'''
        self.over_input ()
        self._t_chars = []
        self._strings = []
        self._cursor = [0,0]
        self._py_mode = False
        self._zi = u''
        self.update_candidates
    
    def is_empty (self):
        return len(self._t_chars) == 0

    def clear_input (self):
        '''
        Remove input characters held for Table mode,
        '''
        self._chars = [[],[],[]]
        self._tabkey_list = []
        self._lookup_table.clean()
        self._lookup_table.show_cursor(False)
        self._candidates = [[],[]]
    
    def over_input (self):
        '''
        Remove input characters held for Table mode,
        '''
        self.clear_input ()
        self._u_chars = []
    
    def set_parser (self, parser):
        '''change input parser'''
        self.clear ()
        self._parser = parser
        
    def add_input (self,c):
        '''add input character'''
        if len (self._t_chars) == self._max_length:
            return True
        self._zi = u''
        if self._cursor[1]:
            self.split_phrase()
        if (len (self._chars[0]) == self._max_key_len and (not self._py_mode)) or ( len (self._chars[0]) == 7 and self._py_mode ) :
            self.auto_commit_to_preedit()
            res = self.add_input (c)
            return res
        elif self._chars[1]:
            self._chars[1].append (c)
        else:
            if (not self._py_mode and ( c in self._valid_input_chars)) or\
                (self._py_mode and (c in u'abcdefghijklmnopqrstuvwxyz!@#$%')):
                try:
                    self._tabkey_list += self._parser (c)
                    self._chars[0].append (c)
                except:
                    self._chars[1].append (c)
            else:
                self._chars[1].append (c)
        self._t_chars.append(c)
        res = self.update_candidates ()
        return res

    def pop_input (self):
        '''remove and display last input char held'''
        _c =''
        if self._chars[1]:
            _c = self._chars[1].pop ()
        elif self._chars[0]:
            _c = self._chars[0].pop ()
            self._tabkey_list.pop()
            if (not self._chars[0]) and self._u_chars:
                self._chars[0] = self._u_chars.pop()
                self._chars[1] = self._chars[1][:-1]
                self._tabkey_list = self._parser (self._chars[0])
                self._strings.pop (self._cursor[0] - 1 )
                self._cursor[0] -= 1
        self._t_chars.pop()
        self.update_candidates ()
        return _c
    
    def get_input_chars (self):
        '''get characters held, valid and invalid'''
        return self._chars[0] + self._chars[1]

    def get_input_chars_string (self):
        '''Get valid input char string'''
        return u''.join(map(str,self._t_chars))

    def get_all_input_strings (self):
        '''Get all uncommit input characters, used in English mode or direct commit'''
        return  u''.join( map(u''.join, self._u_chars + [self._chars[0]] \
            + [self._chars[1]]) )
    
    def get_index(self,key):
        '''Get the index of key in database table'''
        return self._pt.index(key)

    def split_phrase (self):
        '''Splite current phrase into two phrase'''
        _head = u''
        _end = u''
        try:
            _head = self._strings[self._cursor[0]][:self._cursor[1]]
            _end = self._strings[self._cursor[0]][self._cursor[1]:]
            self._strings.pop(self._cursor[0])
            self._strings.insert(self._cursor[0],_head)
            self._strings.insert(self._cursor[0]+1,_end)
            self._cursor[0] +=1
            self._cursor[1] = 0
        except:
            pass
    
    def remove_before_string (self):
        '''Remove string before cursor'''
        if self._cursor[1] != 0:
            self.split_phrase()
        if self._cursor[0] > 0:
            self._strings.pop(self._cursor[0]-1)
            self._cursor[0] -= 1
        else:
            pass
        # if we remove all characters in preedit string, we need to clear the self._t_chars
        if self._cursor == [0,0]:
            self._t_chars =[]
    
    def remove_after_string (self):
        '''Remove string after cursor'''
        if self._cursor[1] != 0:
            self.split_phrase()
        if self._cursor[0] >= len (self._strings):
            pass
        else:
            self._strings.pop(self._cursor[0])
    
    def remove_before_char (self):
        '''Remove character before cursor'''
        if self._cursor[1] > 0:
            _str = self._strings[ self._cursor[0] ]
            self._strings[ self._cursor[0] ] = _str[ : self._cursor[1]-1] + _str[ self._cursor[1] :]
            self._cursor[1] -= 1
        else:
            if self._cursor[0] == 0:
                pass
            else:
                if len ( self._strings[self._cursor[0] - 1] ) == 1:
                    self.remove_before_string()
                else:
                    self._strings[self._cursor[0] - 1] = self._strings[self._cursor[0] - 1][:-1]
        # if we remove all characters in preedit string, we need to clear the self._t_chars
        if self._cursor == [0,0]:
            self._t_chars =[]

    def remove_after_char (self):
        '''Remove character after cursor'''
        if self._cursor[1] == 0:
            if self._cursor[0] == len ( self._strings):
                pass
            else:
                if len( self._strings[ self._cursor[0] ]) == 1:
                    self.remove_after_string ()
                else:
                    self._strings[ self._cursor[0] ] = self._strings[ self._cursor[0] ][1:]
        else:
            if ( self._cursor[1] + 1 ) == len( self._strings[ self._cursor[0] ] ) :
                self.split_phrase ()
                self.remove_after_string ()
            else:
                string = self._strings[ self._cursor[0] ]
                self._strings[ self._cursor[0] ] = string[:self._cursor[1]] + string[ self._cursor[1] + 1 : ]

    def get_invalid_input_chars (self):
        '''get invalid characters held'''
        return self._chars[1]

    def get_invalid_input_string (self):
        '''get invalid characters in string form'''
        return u''.join (self._chars[1])
        
    def get_preedit_strings (self):
        '''Get preedit strings'''
        if self._candidates[0]:
            if self._py_mode:
                _p_index = 8
            else:
                _p_index = self.get_index ('phrase')
            _candi = u'###' + self._candidates[0][ int (self._lookup_table.get_cursor_pos() ) ][ _p_index ] + u'###' 
        else:
            input_chars = self.get_input_chars ()
            if input_chars:
                _candi = u''.join( ['###'] + map( str, input_chars) + ['###'] )
            else:
                _candi = u''
        if self._strings:
            res = u''
            _cursor = self._cursor[0]
            _luc = len (self._u_chars)
            if _luc:
                _candi = _candi == u'' and u'######' or _candi
                res =u''.join( self._strings[ : _cursor - _luc] +[u'@@@'] + self._strings[_cursor - _luc : _cursor ]  + [ _candi  ] + self._strings[ _cursor : ])
            else:
                res = u''.join( self._strings[ : _cursor ] + [ _candi  ] + self._strings[ _cursor : ])
            return res
        else:
            return _candi 
    def add_caret (self, addstr):
        '''add length to caret position'''
        self._caret += len(addstr)

    def get_caret (self):
        '''Get caret position in preedit strings'''
        self._caret = 0
        if self._cursor[0] and self._strings:
            map (self.add_caret,self._strings[:self._cursor[0]])
        self._caret += self._cursor[1]
        if self._candidates[0]:
            if self._py_mode:
                _p_index = 8
            else:
                _p_index = self.get_index ('phrase')
            _candi =self._candidates[0][ int (self._lookup_table.get_cursor_pos() ) ][ _p_index ] 
        else:
            _candi = u''.join( map( str,self.get_input_chars()) )
        self._caret += len( _candi ) 
        return self._caret
    
    def arrow_left (self):
        '''Process Arrow Left Key Event.
        Update cursor data when move caret left'''
        if self.get_preedit_strings ():
            if not( self.get_input_chars () or self._u_chars ):
                if self._cursor[1] > 0:
                    self._cursor[1] -= 1
                else:
                    if self._cursor[0] > 0:
                        self._cursor[1] = len (self._strings[self._cursor[0]-1]) - 1
                        self._cursor[0] -= 1
                    else:
                        self._cursor[0] = len(self._strings)
                        self._cursor[1] = 0
                self.update_candidates ()
            return True
        else:
            return False
    
    def arrow_right (self):
        '''Process Arrow Right Key Event.
        Update cursor data when move caret right'''
        if self.get_preedit_strings ():
            if not( self.get_input_chars () or self._u_chars ):
                if self._cursor[1] == 0:
                    if self._cursor[0] == len (self._strings):
                        self._cursor[0] = 0
                    else:
                        self._cursor[1] += 1
                else:
                    self._cursor[1] += 1
                if self._cursor[1] == len(self._strings[ self._cursor[0] ]):
                    self._cursor[0] += 1
                    self._cursor[1] = 0
                self.update_candidates ()
            return True
        else:
            return False

    def control_arrow_left (self):
        '''Process Control + Arrow Left Key Event.
        Update cursor data when move caret to string left'''
        if self.get_preedit_strings ():
            if not( self.get_input_chars () or self._u_chars ):
                if self._cursor[1] == 0:
                    if self._cursor[0] == 0:
                        self._cursor[0] = len (self._strings) - 1
                    else:
                        self._cursor[0] -= 1
                else:
                    self._cursor[1] = 0
                self.update_candidates ()
            return True
        else:
            return False
    
    def control_arrow_right (self):
        '''Process Control + Arrow Right Key Event.
        Update cursor data when move caret to string right'''
        if self.get_preedit_strings ():
            if not( self.get_input_chars () or self._u_chars ):
                if self._cursor[1] == 0:
                    if self._cursor[0] == len (self._strings):
                        self._cursor[0] = 1
                    else:
                        self._cursor[0] += 1
                else:
                    self._cursor[0] += 1
                    self._cursor[1] = 0
                self.update_candidates ()
            return True
        else:
            return False
    def ap_candidate (self, candi):
        '''append candidate to lookup_table'''
        if not self._py_mode:
            _p_index = self.get_index('phrase')
            _fkey = self.get_index('m0')
        else:
            _p_index = 8
            _fkey = 1
        if self.db._is_chinese:
            _tbks = u''.join( map(self._deparser , candi[_fkey + len(self._tabkey_list) : _p_index-1 ] ) )
            if self._py_mode:
                # restore tune symbol
                _tbks = _tbks.replace('!','↑1').replace('@','↑2').replace('#','↑3').replace('$','↑4').replace('%','↑5')
        else:
            _tbks = u''.join( map(self._deparser , candi[_fkey + len(self._tabkey_list) : _p_index ] ) )
        _phrase = candi[_p_index]
        # further color implementation needed :)
        # here -2 is the pos of num, -1 is the pos of . 0 is the pos of string
        #attrs = ibus.AttrList ([ibus.AttributeForeground (0x8e2626, -2, 1)])
        attrs = ibus.AttrList ()
        # this is the part of tabkey
        attrs.append( ibus.AttributeForeground ( 0x1973a2, 0, \
            len(_phrase) + len(_tbks)))
        if candi[-2] < 0:
            # this is a user defined phrase:
            attrs.append ( ibus.AttributeForeground (0x7700c3, 0, len(_phrase)) )
        elif candi[-1] > 0:
            # this is a sys phrase used by user:
            attrs.append ( ibus.AttributeForeground (0x000000, 0, len(_phrase)) )
        else:
            # this is a system phrase haven't been used:
            attrs.append ( ibus.AttributeForeground (0x000000, 0, len(_phrase)) )
        self._lookup_table.append_candidate ( ibus.Text(_phrase + _tbks, attrs) )
        self._lookup_table.show_cursor (False)

    def filter_candidates (self, candidates):
        '''Filter candidates if IME is Chinese'''
        #if self.db._is_chinese and (not self._py_mode):
        if not self._chinese_mode in(2,3):
            return candidates[:]
        bm_index = self._pt.index('category')
        if self._chinese_mode == 2:
            # big charset with SC first
            return  filter (lambda x: x[bm_index] & 1, candidates)\
                    +filter (lambda x: x[bm_index] & (1 << 1) and \
                            (not x[bm_index] & 1), candidates)\
                    + filter (lambda x: x[bm_index] & (1 << 2), candidates)
        elif self._chinese_mode == 3:
            # big charset with SC first
            return  filter (lambda x: x[bm_index] & (1 << 1), candidates)\
                    +filter (lambda x: x[bm_index] & 1 and\
                    (not x[bm_index] & (1<<1)) , candidates)\
                    + filter (lambda x: x[bm_index] & (1 << 2), candidates)

    def update_candidates (self):
        '''Update lookuptable'''
        # first check whether the IME have defined start_chars
        if self.db.startchars and ( len(self._chars[0]) == 1 )\
                and ( len(self._chars[1]) == 0 ) \
                and ( self._chars[0][0] not in self.db.startchars):
            self._u_chars.append ( self._chars[0][0] )
            self._strings.insert ( self._cursor[0], self._chars[0][0] )
            self._cursor [0] += 1
            self.clear_input()
        else:
            if (self._chars[0] == self._chars[2] and self._candidates[0]) \
                    or self._chars[1]:
                # if no change in valid input char or we have invalid input,
                # we do not do sql enquery
                pass
            else:
                # check whether last time we have only one candidate
                only_one_last = self.one_candidate()
                # do enquiry
                self._lookup_table.clean ()
                self._lookup_table.show_cursor (False)
                if self._tabkey_list:
                    # here we need to consider two parts, table and pinyin
                    # first table
                    if not self._py_mode:
                        if self.db._is_chinese :
                            bm_index = self._pt.index('category')
                            if self._chinese_mode == 0:
                                # simplify Chinese mode
                                self._candidates[0] = self.db.select_words(\
                                        self._tabkey_list, self._onechar, 1 )
                            elif self._chinese_mode == 1:
                                # traditional Chinese mode
                                self._candidates[0] = self.db.select_words(\
                                        self._tabkey_list, self._onechar, 2 )
                            else:
                                self._candidates[0] = self.db.select_words(\
                                        self._tabkey_list, self._onechar )
                        else:
                            self._candidates[0] = self.db.select_words( self._tabkey_list, self._onechar )
                    else:
                        self._candidates[0] = self.db.select_zi( self._tabkey_list )
                    self._chars[2] = self._chars[0][:]
                        
                else:
                    self._candidates[0] =[]
                if self._candidates[0]:
                    self._candidates[0] = self.filter_candidates (self._candidates[0])
                if self._candidates[0]:
                    map ( self.ap_candidate,self._candidates[0] )
                else:
                    if self._chars[0]:
                        ## old manner:
                        #if self._candidates[1]:
                        #    #print self._candidates[1]
                        #    self._candidates[0] = self._candidates[1]
                        #    self._candidates[1] = []
                        #    last_input = self.pop_input ()
                        #    self.auto_commit_to_preedit ()
                        #    res = self.add_input( last_input )
                        #    print res
                        #    return res
                        #else:
                        #    self.pop_input ()
                        #    self._lookup_table.clean()
                        #    self._lookup_table.show_cursor (False)
                        #    return False
                        ###################
                        ## new manner, we add new char to invalid input
                        ## chars
                        if not self._chars[1]:
                            # we don't have invalid input chars
                            # here we need to check the last input
                            # is a punctuation or not, if is a punct,
                            # then we use old maner to summit the former valid
                            # candidate
                            if ascii.ispunct (self._chars[0][-1].encode('ascii')) \
                                    or len (self._chars[0][:-1]) \
                                    in self.db.pkeylens \
                                    or only_one_last:
                                # because we use [!@#$%] to denote [12345]
                                # in py_mode, so we need to distinguish them
                                ## old manner:
                                if self._py_mode:
                                    if self._chars[0][-1] in "!@#$%":
                                        self._chars[0].pop() 
                                        self._tabkey_list.pop()
                                        return True

                                if self._candidates[1]:
                                    self._candidates[0] = self._candidates[1]
                                    self._candidates[1] = []
                                    last_input = self.pop_input ()
                                    self.auto_commit_to_preedit ()
                                    res = self.add_input( last_input )
                                    return res
                                else:
                                    self.pop_input ()
                                    self._lookup_table.clean()
                                    self._lookup_table.show_cursor (False)
                                    return False
                            else:    
                                # this is not a punct or not a valid phrase
                                # last time
                                self._chars[1].append( self._chars[0].pop() )
                                self._tabkey_list.pop()
                        else:
                            pass
                        self._candidates[0] =[]
                    else:
                        self._lookup_table.clean()
                        self._lookup_table.show_cursor (False)
                self._candidates[1] = self._candidates[0]
            
        return True    

    def commit_to_preedit (self):
        '''Add select phrase in lookup table to preedit string'''
        if not self._py_mode:
            _p_index = self.get_index('phrase')
        else:
            _p_index = 8
        try:
            if self._candidates[0]:
                self._strings.insert(self._cursor[0], self._candidates[0][ self.get_cursor_pos() ][_p_index])
                self._cursor [0] += 1
                if self._py_mode:
                    self._zi = self._candidates[0][ self.get_cursor_pos() ][_p_index]
            self.over_input ()
            self.update_candidates ()
        except:
            pass
    
    def auto_commit_to_preedit (self):
        '''Add select phrase in lookup table to preedit string'''
        if not self._py_mode:
            _p_index = self.get_index('phrase')
        else:
            _p_index = 8
        try:
            self._u_chars.append( self._chars[0][:] )
            self._strings.insert(self._cursor[0], self._candidates[0][ self.get_cursor_pos() ][_p_index])
            self._cursor [0] += 1
            self.clear_input()
            self.update_candidates ()
        except:
            pass

    def get_aux_strings (self):
        '''Get aux strings'''
        input_chars = self.get_input_chars ()
        if input_chars:
            #aux_string =  u' '.join( map( u''.join, self._u_chars + [self._chars[0]] ) )
            aux_string =   u''.join (self._chars[0]) 
            if self._py_mode:
                aux_string = aux_string.replace('!','1').replace('@','2').replace('#','3').replace('$','4').replace('%','5')
            return aux_string

        aux_string = u''
        if self._zi:
            # we have pinyin result
            tabcodes = self.db.find_zi_code(self._zi)
            aux_string = self._zi+u': '
            aux_string = u' '.join(tabcodes)
        #    self._zi = u''
        cstr = u''.join(self._strings)
        if self.db.user_can_define_phrase:
            if len (cstr ) > 1:
                aux_string += (u'\t#: ' + self.db.parse_phrase_to_tabkeys (cstr))
        return aux_string
    def arrow_down(self):
        '''Process Arrow Down Key Event
        Move Lookup Table cursor down'''
        res = self._lookup_table.cursor_down()
        self.update_candidates ()
        if not res and self._candidates[0]:
            return True
        return res
    
    def arrow_up(self):
        '''Process Arrow Up Key Event
        Move Lookup Table cursor up'''
        res = self._lookup_table.cursor_up()
        self.update_candidates ()
        if not res and self._candidates[0]:
            return True
        return res
    
    def page_down(self):
        '''Process Page Down Key Event
        Move Lookup Table page down'''
        res = self._lookup_table.page_down()
        self.update_candidates ()
        if not res and self._candidates[0]:
            return True
        return res
    
    def page_up(self):
        '''Process Page Up Key Event
        move Lookup Table page up'''
        res = self._lookup_table.page_up()
        self.update_candidates ()
        if not res and self._candidates[0]:
            return True
        return res
    
    def number (self, index):
        '''Select the candidates in Lookup Table
        index should start from 0'''
        self._lookup_table.set_cursor_pos_in_current_page ( index )
        if index != self._lookup_table.get_cursor_pos_in_current_page ():
            # the index given is out of range we do not commit string
            return False
        self.commit_to_preedit ()
        return True

    def alt_number (self,index):
        '''Remove the candidates in Lookup Table from user_db index should start from 0'''
        cps = self._lookup_table.get_current_page_start()
        pos = cps + index
        if  len (self._candidates[0]) > pos:
            # this index is valid
            can = self._candidates[0][pos]
            if can[-2] < 0:
                # freq of this candidate is -1, means this a user phrase
                self.db.remove_phrase (can)
                # make update_candidates do sql enquiry
                self._chars[2].pop()
                self.update_candidates ()
            return True
        else:
            return False

    def get_cursor_pos (self):
        '''get lookup table cursor position'''
        return self._lookup_table.get_cursor_pos()

    def get_lookup_table (self):
        '''Get lookup table'''
        return self._lookup_table

    def is_lt_visible (self):
        '''Check whether lookup table is visible'''
        return self._lookup_table.is_cursor_visible ()
    
    def backspace (self):
        '''Process backspace Key Event'''
        self._zi = u''
        if self.get_input_chars():
            self.pop_input ()
            return True
        elif self.get_preedit_strings ():
            self.remove_before_char ()
            return True
        else:
            return False
    
    def control_backspace (self):
        '''Process control+backspace Key Event'''
        self._zi = u''
        if self.get_input_chars():
            self.over_input ()
            return True
        elif self.get_preedit_strings ():
            self.remove_before_string ()
            return True
        else:
            return False

    def delete (self):
        '''Process delete Key Event'''
        self._zi = u''
        if self.get_input_chars():
            return True
        elif self.get_preedit_strings ():
            self.remove_after_char ()
            return True
        else:
            return False
    
    def control_delete (self):
        '''Process control+delete Key Event'''
        self._zi = u''
        if self.get_input_chars ():
            return True
        elif self.get_preedit_strings ():
            self.remove_after_string ()
            return True
        else:
            return False

    def l_shift (self):
        '''Process Left Shift Key Event as immediately commit to preedit strings'''
        if self._chars[0]:
            self.commit_to_preedit ()
            return True
        else:
            return False
    
    def r_shift (self):
        '''Proess Right Shift Key Event as changed between PinYin Mode and Table Mode'''
        self._zi = u''
        if self._chars[0]:
            self.commit_to_preedit ()
        self._py_mode = not (self._py_mode)
        return True

    def space (self):
        '''Process space Key Event
        return (KeyProcessResult,whethercommit,commitstring)'''
        if self._chars[1]:
            # we have invalid input, so do not commit 
            return (False,u'')
        if self._t_chars :
            # user has input sth
            istr = self.get_all_input_strings ()
            self.commit_to_preedit ()
            pstr = self.get_preedit_strings ()
            #print "istr: ",istr
            self.clear()
            return (True,pstr,istr)
        else:
            return (False,u'',u'')
    
    def one_candidate (self):
        '''Return true if there is only one candidate'''
        return len(self._candidates[0]) == 1


########################
### Engine Class #####
####################
class tabengine (ibus.EngineBase):
    '''The IM Engine for Tables'''
    
    # colors
#    _phrase_color             = 0xffffff
#    _user_phrase_color         = 0xffffff
#    _new_phrase_color         = 0xffffff

    # lookup table page size
    _page_size = 6

    def __init__ (self, bus, obj_path, db ):
        print 'obj_path is', obj_path
        super(tabengine,self).__init__ (bus,obj_path)
        self._bus = bus

        self.sock = socket(AF_INET, SOCK_STREAM)
        self.sock.connect(("localhost", 12345))
        self.sock = self.sock.makefile("rwb", 0)

        self._compstr = ''
        self._cands = []
        self._hint = ''
        self._commit = ''
        self._candsstr = ''
        self._cand_idx = ''
        self._active = ''

        self._lookup_table = ibus.LookupTable (tabengine._page_size)
        # this is the backend sql db we need for our IME
        # we receive this db from IMEngineFactory
        #self.db = tabsqlitedb.tabsqlitedb( name = dbname )
        self.db = db 
        # this is the parer which parse the input string to key object
        self._parser = tabdict.parse
        
        self._icon_dir = '%s%s%s%s' % (os.getenv('IBUS_TABLE_LOCATION'),
                os.path.sep, 'icons', os.path.sep)
        # 0 = english input mode
        # 1 = table input mode
        self._mode = 1
        # self._ime_py: True / False this IME support pinyin mode
        self._ime_py = self.db.get_ime_property ('pinyin_mode')
        if self._ime_py:
            if self._ime_py.lower() == u'true':
                self._ime_py = True
            else:
                self._ime_py = False
        else:
            print 'We coult not find "pinyin_mode" entry in database, is it a outdated database?'
            self._ime_py = False

        self._status = self.db.get_ime_property('status_prompt').encode('utf8')
        # now we check and update the valid input characters
        self._chars = self.db.get_ime_property('valid_input_chars')
        self._valid_input_chars = []
        for _c in self._chars:
            if _c in tabdict.tab_key_list:
                self._valid_input_chars.append(_c)
        del self._chars

        # check whether we can use '=' and '-' for page_down/up
        self._page_down_keys = [keysyms.Page_Down, keysyms.KP_Page_Down]
        self._page_up_keys = [keysyms.Page_Up, keysyms.KP_Page_Up]
        if '=' not in self._valid_input_chars \
                and '-' not in self._valid_input_chars:
            self._page_down_keys.append (keysyms.equal)
            self._page_up_keys.append (keysyms.minus)
        
        self._pt = self.db.get_phrase_table_index ()
        self._ml = int(self.db.get_ime_property ('max_key_length'))
        
        # name for config section
        self._name = self.db.get_ime_property('name')
        self._config_section = "engine/Table/%s" % self._name
        
        # config module
        self._config = self._bus.get_config ()
        # Containers we used:
        self._editor = editor(self._config, self._pt, self._valid_input_chars, self._ml, self.db)

        # some other vals we used:
        # self._prev_key: hold the key event last time.
        self._prev_key = None
        self._prev_char = None
        self._double_quotation_state = False
        self._single_quotation_state = False

        # [EnMode,TabMode] we get TabMode properties from db
        self._full_width_letter = [
                self._config.get_value (self._config_section,
                    "EnDefFullWidthLetter",
                    False),
                self._config.get_value (self._config_section, 
                    "TabDefFullWidthLetter", 
                    self.db.get_ime_property('def_full_width_letter').lower() == u'true' )
                ]
        self._full_width_punct = [
                self._config.get_value (self._config_section,
                    "EnDefFullWidthPunct",
                    False),
                self._config.get_value (self._config_section, 
                    "TabDefFullWidthPunct", 
                    self.db.get_ime_property('def_full_width_punct').lower() == u'true' )
                ]
        # some properties we will involved, Property is taken from scim.
        #self._setup_property = Property ("setup", _("Setup"))
        try:
            self._auto_commit = self.db.get_ime_property('auto_commit').lower() == u'true'
        except:
            self._auto_commit = False
        self._auto_commit = self._config.get_value (self._config_section, "AutoCommit",
                self._auto_commit)
        # the commit phrases length
        self._len_list = [0]
        # connect to SpeedMeter
        #try:
        #    bus = dbus.SessionBus()
        #    user = os.path.basename( os.path.expanduser('~') )
        #    self._sm_bus = bus.get_object ("org.ibus.table.SpeedMeter.%s"\
        #            % user, "/org/ibus/table/SpeedMeter")
        #    self._sm =  dbus.Interface(self._sm_bus,\
        #            "org.ibus.table.SpeedMeter") 
        #except:
        #    self._sm = None
        #self._sm_on = False
        self._on = False
        self.reset ()

    def reset (self):
        self._editor.clear ()
        self._double_quotation_state = False
        self._single_quotation_state = False
        self._prev_key = None
        #self._editor._onechar = False    
        self._init_properties ()
        self._update_ui ()
    
    def do_destroy(self):
        self.reset ()
        self.focus_out ()
        #self.db.sync_usrdb ()
        super(tabengine,self).do_destroy()

    def _init_properties (self):
        self.properties= ibus.PropList ()
        self._status_property = ibus.Property(u'status')
        if self.db._is_chinese:
            self._cmode_property = ibus.Property(u'cmode')
        self._letter_property = ibus.Property(u'letter')
        self._punct_property = ibus.Property(u'punct')
        self._py_property = ibus.Property(u'py_mode')
        self._onechar_property = ibus.Property(u'onechar')
        self._auto_commit_property = ibus.Property(u'acommit')
        for prop in (self._status_property,
            self._letter_property,
            self._punct_property,
            self._py_property,
            self._onechar_property,
            self._auto_commit_property
            #self._setup_property
            ):
            self.properties.append(prop)
        if self.db._is_chinese:
            self.properties.insert( 1, self._cmode_property )
        self.register_properties (self.properties)
        self._refresh_properties ()
    
    def _refresh_properties (self):
        '''Method used to update properties'''
        # taken and modified from PinYin.py :)
        if self._mode == 1: # refresh mode
            if self._status == u'CN':
                self._status_property.set_icon( u'%s%s' % (self._icon_dir, 'chinese.svg') )
                self._status_property.set_label(  _(u'CN') )
            else:
                self._status_property.set_icon( u'%s%s' % (self._icon_dir, 'ibus-table.svg') )
                self._status_property.set_label(  self._status )
            self._status_property.set_tooltip (  _(u'Switch to English mode') )
        else:
            self._status_property.set_icon( u'%s%s' % (self._icon_dir, 'english.svg') )
            self._status_property.set_label( _(u'EN') )
            self._status_property.set_tooltip (  _(u'Switch to Table mode') )

        if self._full_width_letter[self._mode]:
            self._letter_property.set_icon ( u'%s%s' % (self._icon_dir, 'full-letter.svg') )
            self._letter_property.set_tooltip ( _(u'Switch to half letter') )
        else:
            self._letter_property.set_icon ( u'%s%s' % (self._icon_dir, 'half-letter.svg') )
            self._letter_property.set_tooltip ( _(u'Switch to full letter') )

        if self._full_width_punct[self._mode]:
            self._punct_property.set_icon ( u'%s%s' % (self._icon_dir, 'full-punct.svg') )
            self._punct_property.set_tooltip ( _( u'Switch to half punction' ) )
        else:
            self._punct_property.set_icon ( u'%s%s' % (self._icon_dir,'half-punct.svg' ) )
            self._punct_property.set_tooltip ( _( u'Switch to full punction' ) )
        
        if self._editor._py_mode:
            self._py_property.set_icon ( u'%s%s' % (self._icon_dir, 'py-mode.svg' ) )
            self._py_property.set_tooltip ( _(u'Switch to Table mode') )
        
        else:
            self._py_property.set_icon ( u'%s%s' % (self._icon_dir, 'tab-mode.svg' ) )
            self._py_property.set_tooltip ( _(u'Switch to PinYin mode') )

        if self._editor._onechar:
            self._onechar_property.set_icon ( u'%s%s' % (self._icon_dir, 'onechar.svg' ))
            self._onechar_property.set_tooltip ( _(u'Switch to phrase mode') )
        else:
            self._onechar_property.set_icon ( u'%s%s' % (self._icon_dir, 'phrase.svg' ))
            self._onechar_property.set_tooltip ( _(u'Switch to single char mode') )
        if self._auto_commit:
            self._auto_commit_property.set_icon ( u'%s%s' % (self._icon_dir, 'acommit.svg' ) ) 
            self._auto_commit_property.set_tooltip ( _(u'Switch to normal commit mode, which use space to commit') ) 
        else:
            self._auto_commit_property.set_icon ( u'%s%s' % (self._icon_dir, 'ncommit.svg' ) ) 
            self._auto_commit_property.set_tooltip ( _(u'Switch to direct commit mode') ) 
        # the chinese_mode:
        if self.db._is_chinese:
            if self._editor._chinese_mode == 0:
                self._cmode_property.set_icon ( u'%s%s' % (self._icon_dir,\
                        'sc-mode.svg' ) ) 
                self._cmode_property.set_tooltip ( _(u'Switch to Traditional Chinese mode') ) 
            elif self._editor._chinese_mode == 1:
                self._cmode_property.set_icon ( u'%s%s' % (self._icon_dir,\
                        'tc-mode.svg' ) ) 
                self._cmode_property.set_tooltip ( _(u'Switch to Simplify Chinese first Big Charset Mode') ) 
            elif self._editor._chinese_mode == 2:
                self._cmode_property.set_icon ( u'%s%s' % (self._icon_dir,\
                        'scb-mode.svg' ) ) 
                self._cmode_property.set_tooltip ( _(u'Switch to Traditional Chinese first Big Charset Mode') ) 
            elif self._editor._chinese_mode == 3:
                self._cmode_property.set_icon ( u'%s%s' % (self._icon_dir,\
                        'tcb-mode.svg' ) ) 
                self._cmode_property.set_tooltip ( _(u'Switch to Big Charset Mode') ) 
            elif self._editor._chinese_mode == 4:
                self._cmode_property.set_icon ( u'%s%s' % (self._icon_dir,\
                        'cb-mode.svg' ) ) 
                self._cmode_property.set_tooltip ( _(u'Switch to Simplify Chinese Mode') ) 

        # use buildin method to update properties :)
        map (self.update_property, self.properties)
    
    def _change_mode (self):
        '''Shift input mode, TAB -> EN -> TAB
        '''
        self._mode = int (not self._mode)
        self.reset ()
        self._update_ui ()

    def property_activate (self, property,prop_state = ibus.PROP_STATE_UNCHECKED):
        '''Shift property'''
        if property == u"status":
            self._change_mode ()
        elif property == u'py_mode' and self._ime_py:
            self._editor.r_shift ()
        elif property == u'onechar':
            self._editor._onechar = not self._editor._onechar
            self._config.set_value( self._config_section,
                    "OneChar",
                    self._editor._onechar)

        elif property == u'acommit':
            self._auto_commit = not self._auto_commit
            self._config.set_value( self._config_section,
                    "AutoCommit",
                    self._auto_commit)
        elif property == u'letter':
            self._full_width_letter [self._mode] = not self._full_width_letter [self._mode]
            if self._mode:
                self._config.set_value( self._config_section,
                        "TabDefFullWidthLetter",
                        self._full_width_letter [self._mode])
            else:
                self._config.set_value( self._config_section,
                        "EnDefFullWidthLetter",
                        self._full_width_letter [self._mode])

        elif property == u'punct':
            self._full_width_punct [self._mode] = not self._full_width_punct [self._mode]
            if self._mode:
                self._config.set_value( self._config_section,
                        "TabDefFullWidthPunct",
                        self._full_width_punct [self._mode])
            else:
                self._config.set_value( self._config_section,
                        "EnDefFullWidthPunct",
                        self._full_width_punct [self._mode])
        elif property == u'cmode':
            self._editor.change_chinese_mode()
            self.reset()
        self._refresh_properties ()
    #    elif property == "setup":
            # Need implementation
    #        self.start_helper ("96c07b6f-0c3d-4403-ab57-908dd9b8d513")
        # at last invoke default method 
    
    def _update_preedit (self):
        '''Update Preedit String in UI'''
        _str = self._editor.get_preedit_strings ()
        if _str == u'':
            super(tabengine, self).update_preedit_text(ibus.Text(u'',None), 0, False)
        else:
            attrs = ibus.AttrList()
            res = patt_edit.match (_str)
            if res:
                _str = u''
                ures = patt_uncommit.match (res.group(1))
                if ures:
                    _str=u''.join (ures.groups())
                    lc = len (ures.group(1) )
                    lu = len (ures.group(2) )
                    attrs.append (ibus.AttributeForeground(0x1b3f03,0,lc) )
                    attrs.append (ibus.AttributeForeground(0x0895a2,lc,lu) )
                    lg1 = len (_str)
                else:
                    _str += res.group (1)
                    lg1 = len ( res.group(1) )
                    attrs.append (ibus.AttributeForeground(0x1b3f03,0,lg1) )
                _str += res.group(2)
                _str += res.group(3)
                lg2 = len ( res.group(2) )
                lg3 = len ( res.group(3) )
                attrs.append( ibus.AttributeForeground(0x0e0ea0,lg1,lg2) )
                attrs.append( ibus.AttributeForeground(0x1b3f03,lg1+lg2,lg3) )
            else:
                attrs.append( ibus.AttributeForeground(0x1b3f03,0,len(_str)) )
            # because ibus now can only insert preedit into txt, so...
            attrs = ibus.AttrList()
            attrs.append(ibus.AttributeUnderline(ibus.ATTR_UNDERLINE_SINGLE, 0, len(_str)))


            super(tabengine, self).update_preedit_text(ibus.Text(_str, attrs), self._editor.get_caret(), True)
    
    def _update_aux (self):
        '''Update Aux String in UI'''
        _ic = self._editor.get_aux_strings ()
        if _ic:
            attrs = ibus.AttrList([ ibus.AttributeForeground(0x9515b5,0, len(_ic)) ])
            #attrs = [ scim.Attribute(0,len(_ic),scim.ATTR_FOREGROUND,0x5540c1)]

            super(tabengine, self).update_auxiliary_text(ibus.Text(_ic, attrs), True)
        else:
            self.hide_auxiliary_text()
            #self.update_aux_string (u'', None, False)

    def _update_lookup_table (self):
        '''Update Lookup Table in UI'''
        if self._editor.is_empty ():
            self.hide_lookup_table()
            return
        self.update_lookup_table ( self._editor.get_lookup_table(), True, True )    

    def _update_ui (self):
        '''Update User Interface'''
        self._update_lookup_table ()
        self._update_preedit ()
        self._update_aux ()

    #def add_string_len(self, astring):
    #    if self._sm_on:
    #        try:
    #            self._sm.Accumulate(len(astring))
    #        except:
    #            pass
    
    def commit_string (self,string):
        self._editor.clear ()
        self._update_ui ()
        super(tabengine,self).commit_text ( ibus.Text(string) )
        self._prev_char = string[-1]

    def _convert_to_full_width (self, c):
        '''convert half width character to full width'''
        if c in [u".", u"\\", u"^", u"_", u"$", u"\"", u"'", u">", u"<" ]:
            if c == u".":
                if self._prev_char and self._prev_char.isdigit () \
                    and self._prev_key and chr (self._prev_key.code) == self._prev_char:
                    return u"."
                else:
                    return u"\u3002"
            elif c == u"\\":
                return u"\u3001"
            elif c == u"^":
                return u"\u2026\u2026"
            elif c == u"_":
                return u"\u2014\u2014"
            elif c == u"$":
                return u"\uffe5"
            elif c == u"\"":
                self._double_quotation_state = not self._double_quotation_state
                if self._double_quotation_state:
                    return u"\u201c"
                else:
                    return u"\u201d"
            elif c == u"'":
                self._single_quotation_state = not self._single_quotation_state
                if self._single_quotation_state:
                    return u"\u2018"
                else:
                    return u"\u2019"
            elif c == u"<":
                if self._mode:
                    return u"\u300a"
            elif c == u">":
                if self._mode:
                    return u"\u300b"
            
        return ibus.unichar_half_to_full (c)
    
    def _match_hotkey (self, key, code, mask):
        
        if key.code == code and key.mask == mask:
            if self._prev_key and key.code == self._prev_key.code and key.mask & modifier.RELEASE_MASK:
                return True
            if not key.mask & modifier.RELEASE_MASK:
                return True

        return False
    
    def process_key_event(self, keyval, keycode, state):
        '''Process Key Events
        Key Events include Key Press and Key Release,
        modifier means Key Pressed
        '''
        key = KeyEvent(keyval, state & modifier.RELEASE_MASK == 0, state)
        # ignore NumLock mask
        key.mask &= ~modifier.MOD2_MASK

        result = self._process_key_event (key)
        self._prev_key = key
        return result

    def _process_key_event (self, key):
        '''Internal method to process key event'''
        key = str(key)
        print "keyed", key
        if key == '':
            return False
        if self._compstr == '' and len(key) != 1:
            return False

        self.sock.write("keyed " + key + "\n")
        while True:
            line = self.sock.readline()
            if not line:
                break
            line = line[:-1]
            print line, "\n"
            
            if line.find('commit: ') == 0:
                self._commit = line[len('commit: '):]
            
            elif line.find('hint: ') == 0:
                self._hint = line[len('hint: '):]
            
            elif line.find('comp: ') == 0:
                self._compstr = line[len('comp: '):]

            elif line.find('cands: ') == 0:
                self._candsstr = line[len('cands: '):]

            elif line.find('cand_index: ') == 0:
                self._cand_idx = line[len('cand_index: '):]

            elif line.find('active: ') == 0:
                self._active = line[len('active: '):]
            
            elif line == "end:":
                break
            
            else:
                self._hint = line

    def focus_in (self):
        if self._on:
            self.register_properties (self.properties)
            self._refresh_properties ()
            self._update_ui ()
    
    def focus_out (self):
        pass

    def enable (self):
        self._on = True
        self.focus_in()

    def disable (self):
        self.reset()
        self._on = False


    def lookup_table_page_up (self):
        if self._editor.page_up ():
            self._update_lookup_table ()
            return True
        return True

    def lookup_table_page_down (self):
        if self._editor.page_down ():
            self._update_lookup_table ()
            return True
        return False

    # for further implementation :)
    @classmethod
    def CONFIG_VALUE_CHANGED(cls, bus, section, name, value):
        config = bus.get_config()
        if section != self._config_section:
            return
    
    @classmethod
    def CONFIG_RELOADED(cls, bus):
        config = bus.get_config()
        if section != self._config_section:
            return
