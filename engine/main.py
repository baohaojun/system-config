# -*- coding: utf-8 -*-
#
# ibus-sdim - The Sdims engine for IBus
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

import os
import sys
import optparse
import ibus
import gobject
import re
patt = re.compile (r'<\?.*\?>\n')
from signal import signal, SIGTERM, SIGINT

import factory

try:
    icon_dir = os.path.join (os.getenv('IBUS_TABLE_LOCATION'),'icons')
except:
    icon_dir = "/usr/share/ibus-sdim/icons"


opt = optparse.OptionParser()

opt.add_option('--daemon','-d',
        action = 'store_true',dest = 'daemon',default=False,
        help = 'Run as daemon, default: %default')
opt.add_option('--ibus', '-i',
        action = 'store_true',dest = 'ibus',default = False,
        help = 'Set the IME icon file, default: %default')
opt.add_option('--xml', '-x',
        action = 'store_true',dest = 'xml',default = False,
        help = 'output the engines xml part, default: %default')

opt.add_option('--no-debug', '-n',
        action = 'store_false',dest = 'debug',default = True,
        help = 'redirect stdout and stderr to ~/.ibus/sdims/debug.log, default: %default')

(options, args) = opt.parse_args()

if (not options.xml) and options.debug:
    if not os.access ( os.path.expanduser('~/.ibus/sdims'), os.F_OK):
        os.system ('mkdir -p ~/.ibus/sdims')
    logfile = os.path.expanduser('~/.ibus/sdims/debug.log')
    sys.stdout = open (logfile,'a',0)
    sys.stderr = open (logfile,'a',0)
    from time import strftime
    print '--- ', strftime('%Y-%m-%d: %H:%M:%S'), ' ---'



class IMApp:
    def __init__(self, exec_by_ibus):
        self.__mainloop = gobject.MainLoop()
        self.__bus = ibus.Bus()
        self.__bus.connect("disconnected", self.__bus_destroy_cb)
        self.__factory = factory.EngineFactory(self.__bus)
        self.destroied = False
        if exec_by_ibus:
            self.__bus.request_name("org.freedesktop.IBus.Sdim", 0)
        else:
            self.__component = ibus.Component("org.freedesktop.IBus.Sdim",
                                              "Sdim Component",
                                              "0.1.0",
                                              "GPL",
                                              "Yuwei Yu <acevery@gmail.com>")
            name = 'SDIM'
            longname = name
            description = 'Shadow Dance Input Method'
            language = 'zh'
            license = 'LGPL'
            author = 'Bao Haojun <baohaojun@gmail.com>'
            icon = icon_dir + "/sdim.svg"
            if icon:
                icon = os.path.join (icon_dir, icon)
                if not os.access( icon, os.F_OK):
                    icon = ''
            layout = 'us'
            
            self.__component.add_engine(name,
                                        longname,
                                        description,
                                        language,
                                        license,
                                        author,
                                        icon,
                                        layout)
            self.__bus.register_component(self.__component)


    def run(self):
        self.__mainloop.run()
        self.__bus_destroy_cb()

    def quit(self):
        self.__bus_destroy_cb()

    def __bus_destroy_cb(self, bus=None):
        if self.destroied:
            return
        print "finalizing:)"
        self.__factory.do_destroy()
        self.destroied = True
        self.__mainloop.quit()

def cleanup (ima_ins):
    ima_ins.quit()
    sys.exit()

def indent(elem, level=0):
    '''Use to format xml Element pretty :)'''
    i = "\n" + level*"    "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "    "
        for e in elem:
            indent(e, level+1)
            if not e.tail or not e.tail.strip():
                e.tail = i + "    "
        if not e.tail or not e.tail.strip():
            e.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i

def main():
    if options.xml:
        print """
<engines>
    <engine>
        <name>SDIM</name>
        <longname>SDIM</longname>
        <language>zh</language>
        <license>LGPL</license>
        <author>Bao Haojun &lt;baohaojun@gmail.com&gt;</author>
        <icon>%s</icon>
        <layout>us</layout>
        <description>Shadow Dance Input Method</description>
    </engine>
</engines>
""" % (icon_dir + "/sdim.svg")
        return 0

    if options.daemon :
        if os.fork():
                sys.exit()
    ima=IMApp(options.ibus)
    signal (SIGTERM, lambda signum, stack_frame: cleanup(ima))
    signal (SIGINT, lambda signum, stack_frame: cleanup(ima))
    try:
        ima.run()
    except KeyboardInterrupt:
        ima.quit()

if __name__ == "__main__":
    main()

