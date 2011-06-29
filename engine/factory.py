# -*- coding: utf-8 -*-
# vim:et sw=4 sts=4 sw=4
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

import ibus
from ibus.exception import *
import table
import tabsqlitedb
import os
import dbus
from re import compile as re_compile
import sys
import traceback

path_patt = re_compile(r'[^a-zA-Z0-9_/]')

from gettext import dgettext
_  = lambda a : dgettext ("ibus-table", a)
N_ = lambda a : a

engine_base_path = "/com/redhat/IBus/engines/sdim/%s/engine/"


class EngineFactory (ibus.EngineFactoryBase):
    """Table IM Engine Factory"""
    def __init__ (self, bus, db="", icon=""):
        # here the db should be the abs path to sql db
        # this is the backend sql db we need for our IME
        # we need get lots of IME property from this db :)
        #self.db = tabsqlitedb.tabsqlitedb( name = database )
        
        if db:
            self.dbusname = os.path.basename(db).replace('.db','')
            udb = os.path.basename(db).replace('.db','-user.db') 
            self.db = tabsqlitedb.tabsqlitedb( name = db,user_db = udb )
            self.db.db.commit()
            self.dbdict = {self.dbusname:self.db}
        else:
            self.db = None
            self.dbdict = {}

        
        # init factory
        self.bus = bus
        super(EngineFactory,self).__init__ (bus)
        self.engine_id=0
        try:
            bus = dbus.Bus()
            user = os.path.basename( os.path.expanduser('~') )
            self._sm_bus = bus.get_object ("org.ibus.table.SpeedMeter.%s"\
                    % user, "/org/ibus/table/SpeedMeter")
            self._sm =  dbus.Interface(self._sm_bus,\
                    "org.ibus.table.SpeedMeter") 
            print 'self._sm is', self._sm
        except:
            traceback.print_tb(sys.exc_info()[2])
            self._sm = None
    
    def create_engine(self, engine_name):
        # because we need db to be past to Engine
        # the type (engine_name) == dbus.String
        print 'hello world, engine is', engine_name
        name = engine_name.encode ('utf8')
        self.engine_path = engine_base_path % path_patt.sub ('_', name)
        try:
            if not self.db:
                # first check self.dbdict
                if not name in self.dbdict:
                    try:
                        db_dir = os.path.join (os.getenv('IBUS_TABLE_LOCATION'),'tables')
                    except:
                        db_dir = "/usr/share/ibus-table/tables"
                    db = os.path.join (db_dir,name+'.db')
                    udb = name+'-user.db'
                    _sq_db = tabsqlitedb.tabsqlitedb( name = db,user_db = udb )
                    _sq_db.db.commit()
                    self.dbdict[name] = _sq_db
            else:
                name = self.dbusname

            engine = table.tabengine(self.bus, self.engine_path \
                    + str(self.engine_id), self.dbdict[name])
            self.engine_id += 1
            #return engine.get_dbus_object()
            return engine
        except:
            print "fail to create engine %s" % engine_name
            import traceback
            traceback.print_exc ()
            raise IBusException("Can not create engine %s" % engine_name)

    def do_destroy (self):
        '''Destructor, which finish some task for IME'''
        # 
        ## we need to sync the temp userdb in memory to the user_db on disk
        for _db in self.dbdict:
            self.dbdict[_db].sync_usrdb ()
        ##print "Have synced user db\n"
        try:
            self._sm.Quit()
        except:
            traceback.print_tb(sys.exc_info()[2])
        super(EngineFactory,self).do_destroy()


