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

def _str_percent_decode(str):
    return str.replace("%20", " ").replace("%25", "%")


import os
import ibus
#from ibus import Property
from ibus import keysyms
from ibus import modifier
from ibus import ascii
import re

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


class tabengine (ibus.EngineBase):
    '''The IM Engine for Tables'''

    _page_size = 10

    def __init__ (self, bus, obj_path, db):
        print 'obj_path is', obj_path
        super(tabengine,self).__init__ (bus,obj_path)
        self._bus = bus

        self.sock = socket(AF_INET, SOCK_STREAM)
        self.sock.connect(("localhost", 12345))
        self.sock = self.sock.makefile("rwb", 0)

        self.clear_data()
        self._lookup_table = ibus.LookupTable (tabengine._page_size)

        self._name = 'sdim'
        print 'name is', self._name
        self._config_section = "engine/%s" % self._name
        
        # config module
        self._config = self._bus.get_config ()
        self.reset ()

    def clear_data(self):
        self._preedit_str = ''
        self._cands = []
        self._aux_str = ''
        self._commit_str = ''
        self._cands_str = ''
        self._cand_idx = '0'
        self._active = ''

    def reset (self):
        self._update_ui ()
    
    def do_destroy(self):
        self.reset ()
        self.focus_out ()
        super(tabengine,self).do_destroy()

    def _update_preedit (self):
        '''Update Preedit String in UI'''
        _str = self._preedit_str
        if _str == u'':
            super(tabengine, self).update_preedit_text(ibus.Text(u'',None), 0, False)
        else:
            attrs = ibus.AttrList()
            attrs.append( ibus.AttributeForeground(0x1b3f03,0,len(_str)) )
            # because ibus now can only insert preedit into txt, so...
            attrs = ibus.AttrList()
            attrs.append(ibus.AttributeUnderline(ibus.ATTR_UNDERLINE_SINGLE, 0, len(_str)))


            super(tabengine, self).update_preedit_text(ibus.Text(_str, attrs), len(_str), True)
    
    def _update_aux (self):
        '''Update Aux String in UI'''
        _aux = self._aux_str
        if _aux:
            attrs = ibus.AttrList([ ibus.AttributeForeground(0x9515b5, 0, len(_aux)) ])
            super(tabengine, self).update_auxiliary_text(ibus.Text(_aux, attrs), True)
        else:
            self.hide_auxiliary_text()


    def _update_lookup_table (self):
        '''Update Lookup Table in UI'''
        if self._cands_str == '':
            self.hide_lookup_table()
            return

        _cands = self._cands_str.split()
        _cands = [_str_percent_decode(str) for str in _cands]
        
        self._lookup_table.clean()

        for cand in _cands:
            self._lookup_table.append_candidate(ibus.Text(cand, None))

        index = int(self._cand_idx) % 10
        self._lookup_table.set_cursor_pos_in_current_page(index)
        self._lookup_table.show_cursor(True)
        self.update_lookup_table ( self._lookup_table, True, True )    

    def _update_ui (self):
        '''Update User Interface'''
        self._update_lookup_table ()
        self._update_preedit ()
        self._update_aux ()
        self.commit_string()

    #def add_string_len(self, astring):
    #    if self._sm_on:
    #        try:
    #            self._sm.Accumulate(len(astring))
    #        except:
    #            pass
    
    def commit_string (self):
        if self._commit_str == '':
            return
        commit = self._commit_str
        self._commit_str = ''
        super(tabengine,self).commit_text(ibus.Text(commit))

    def process_key_event(self, keyval, keycode, state):
        '''Process Key Events
        Key Events include Key Press and Key Release,
        modifier means Key Pressed
        '''
        key = KeyEvent(keyval, state & modifier.RELEASE_MASK == 0, state)
        # ignore NumLock mask
        key.mask &= ~modifier.MOD2_MASK

        result = self._process_key_event (key)
        return result

    def _process_key_event (self, key):
        '''Internal method to process key event'''
        key = str(key)
        if key == '':
            return False
        if self._preedit_str == '' and len(key) != 1:
            return False
        self._really_process_key(key)
        return True

    def _really_process_key (self, key):
        self.sock.write("keyed " + key + "\n")
        self.clear_data()
        while True:
            line = self.sock.readline()
            if not line:
                break
            line = line[:-1]
            
            if line.find('commit: ') == 0:
                self._commit_str = line[len('commit: '):]
            
            elif line.find('hint: ') == 0:
                self._aux_str = line[len('hint: '):]
            
            elif line.find('comp: ') == 0:
                self._preedit_str = line[len('comp: '):]

            elif line.find('cands: ') == 0:
                self._cands_str = line[len('cands: '):]

            elif line.find('cand_index: ') == 0:
                self._cand_idx = line[len('cand_index: '):]

            elif line.find('active: ') == 0:
                self._active = line[len('active: '):]
            
            elif line == "end:":
                break
            
            else:
                self._aux_str = line
                
            self._update_ui()


    def focus_in (self):
        if self._on:
            self._update_ui ()
    
    def focus_out (self):
        pass

    def enable (self):
        self._on = True
        self.focus_in()

    def disable (self):
        self.reset()
        self._on = False

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
