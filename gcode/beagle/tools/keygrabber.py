#!/usr/bin/env python
# -*- coding: UTF-8 -*-

# This program and all files included in this package are free software;
# you can redistribute it and/or modify it under the terms
# of the GNU General Public License as published by the Free
# Software Foundation; either version 3 of the License,
# or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#
# Authors: Quinn Storm (quinn@beryl-project.org)
#          Patrick Niklaus (marex@opencompositing.org)
#          Guillaume Seguin (guillaume@segu.in)
#          Christopher Williams (christopherw@verizon.net)
# Copyright (C) 2007 Quinn Storm

import sys
import pygtk
import gtk
import gtk.gdk
import gobject
pygtk.require('2.0')
import gtk
import time
import gettext

KeyModifier = ["Shift", "Control", "Mod1", "Mod2", "Mod3", "Mod4",
               "Mod5", "Alt", "Meta", "Super", "Hyper", "ModeSwitch"]

class GrabberWindow:

    def delete_event(self, widget, event, data=None):
        return False

    def destroy(self, widget, data=None):
        gtk.main_quit()

    def __init__(self):
        # create a new window
        self.window = gtk.Window(gtk.WINDOW_TOPLEVEL)
        self.window.connect("delete_event", self.delete_event)
        self.window.connect("destroy", self.destroy)

        self.window.set_border_width(10)
	self.window.set_title("KeyGrabber")
	self.window.set_position(gtk.WIN_POS_MOUSE)
    
        self.button = KeyGrabber(label = _("Grab key combination"))

        self.button.hide()
        self.button.set_no_show_all(True)
        self.button.connect('changed', self.GotKey)

	self.shortcut = ""

        # This packs the button into the window (a GTK container).
        self.window.add(self.button)
    
        # The final step is to display this newly created widget.
        self.button.show()
    
        # and the window
        self.window.show()

    def GotKey(self, widget, key, mods):
        self.shortcut = gtk.accelerator_name (key, mods)
        for mod in KeyModifier:
            if "%s_L" % mod in self.shortcut:
                self.shortcut = self.shortcut.replace ("%s_L" % mod, "<%s>" % mod)
            if "%s_R" % mod in self.shortcut:
                self.shortcut = self.shortcut.replace ("%s_R" % mod, "<%s>" % mod)

        gtk.main_quit()

    def main(self):
        gtk.main()

# Popup
#
class Popup (gtk.Window):

    def __init__ (self, parent, text=None, child=None, decorated=True, mouse=False, modal=True):
        gtk.Window.__init__ (self, gtk.WINDOW_TOPLEVEL)
        self.set_type_hint (gtk.gdk.WINDOW_TYPE_HINT_UTILITY)
        self.set_position (mouse and gtk.WIN_POS_MOUSE or gtk.WIN_POS_CENTER_ALWAYS)
        self.set_transient_for (parent.get_toplevel ())
        self.set_modal (modal)
        self.set_decorated (decorated)
        if text:
            label = gtk.Label (text)
            align = gtk.Alignment ()
            align.set_padding (20, 20, 20, 20)
            align.add (label)
            self.add (align)
        elif child:
            self.add (child)
        gtk_process_events ()

    def destroy (self):
        gtk.Window.destroy (self)
        gtk_process_events ()

# Key Grabber
#
class KeyGrabber (gtk.Button):

    __gsignals__    = {"changed" : (gobject.SIGNAL_RUN_FIRST,
                                    gobject.TYPE_NONE,
                                    [gobject.TYPE_INT, gobject.TYPE_INT]),
                       "current-changed" : (gobject.SIGNAL_RUN_FIRST,
                                    gobject.TYPE_NONE,
                                    [gobject.TYPE_INT, gobject.TYPE_INT])}

    key     = 0
    mods    = 0
    handler = None
    popup   = None

    label   = None

    def __init__ (self, key = 0, mods = 0, label = None):
        '''Prepare widget'''
        super (KeyGrabber, self).__init__ ()

        self.key = key
        self.mods = mods

        self.label = label

        self.connect ("clicked", self.begin_key_grab)
        self.set_label ()

    def begin_key_grab (self, widget):
        self.add_events (gtk.gdk.KEY_PRESS_MASK)
        self.popup = Popup (self, _("Please press the new key combination"))
        self.popup.show_all()
        self.handler = self.popup.connect ("key-press-event",
                                           self.on_key_press_event)
        while gtk.gdk.keyboard_grab (self.popup.window) != gtk.gdk.GRAB_SUCCESS:
            time.sleep (0.1)

    def end_key_grab (self):
        gtk.gdk.keyboard_ungrab (gtk.get_current_event_time ())
        self.popup.disconnect (self.handler)
        self.popup.destroy ()

    def on_key_press_event (self, widget, event):
        mods = event.state & gtk.accelerator_get_default_mod_mask ()

        if event.keyval in (gtk.keysyms.Escape, gtk.keysyms.Return) \
            and not mods:
            if event.keyval == gtk.keysyms.Escape:
                self.emit ("changed", self.key, self.mods)
            self.end_key_grab ()
            self.set_label ()
            return

        key = gtk.gdk.keyval_to_lower (event.keyval)
        if (key == gtk.keysyms.ISO_Left_Tab):
            key = gtk.keysyms.Tab

        if gtk.accelerator_valid (key, mods) \
           or (key == gtk.keysyms.Tab and mods):
            self.set_label (key, mods)
            self.end_key_grab ()
            self.key = key
            self.mods = mods
            self.emit ("changed", self.key, self.mods)
            return

        self.set_label (key, mods)

    def set_label (self, key = None, mods = None):
        if self.label:
            if key != None and mods != None:
                self.emit ("current-changed", key, mods)
            gtk.Button.set_label (self, self.label)
            return
        if key == None and mods == None:
            key = self.key
            mods = self.mods
        label = gtk.accelerator_name (key, mods)
        if not len (label):
            label = _("Disabled")
        gtk.Button.set_label (self, label)

def gtk_process_events ():
    while gtk.events_pending ():
        gtk.main_iteration ()

if __name__ == "__main__":
    if len (sys.argv) == 2:
	gettext.install ('beagle', sys.argv [1])
    else:
	gettext.install ('beagle')
    window = GrabberWindow ()
    window.main ()

    # print the shortcut if the program successfully runs
    print window.shortcut
