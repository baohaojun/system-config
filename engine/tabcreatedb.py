#!/usr/bin/python
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

import os
import sys
sys.path.append( os.path.dirname(os.path.abspath(__file__)) )
import tabsqlitedb
import bz2
import re

from optparse import OptionParser
# we use OptionParser to parse the cmd arguments :)
opt_parser = OptionParser()

opt_parser.add_option( '-n', '--name',
        action = 'store', dest='name',default = None,
        help = 'set the database name we will use, default is %default')

opt_parser.add_option( '-s', '--source',
        action = 'store', dest='source', default = 'xingma.txt.bz2',
        help = 'tell me which file is the source file of IME, default is %default')

opt_parser.add_option( '-e', '--extra',
        action = 'store', dest='extra', default = '',
        help = 'tell me which file is the extra words file for IME, default is %default')

opt_parser.add_option( '-p', '--pinyin',
        action = 'store', dest='pinyin', default = '/usr/share/ibus-table/data/pinyin_table.txt.bz2',
        help = 'tell me which file is the source file of pinyin, default is %default')

opt_parser.add_option( '-o', '--no-create-index',
        action = 'store_false', dest='index', default = True,
        help = 'do not create index on database, only for distrubution purpose, normal user should not invoke this flag!')

opt_parser.add_option( '-i', '--create-index-only',
        action = 'store_true', dest='only_index', default = False,
        help = 'only create index on exist database')

opt_parser.add_option( '-d', '--debug',
        action = 'store_true', dest='debug', default = False,
        help = 'print extra debug messages')



opts,args = opt_parser.parse_args()
if not opts.name and opts.only_index:
    print 'Please give me the database you want to create index on'
    sys.exit(2)

if not opts.name:
    opts.name = os.path.basename(opts.source).split('.')[0] + '.db'


def main ():
    def debug_print ( message ):
        if opts.debug:
            print message
    
    if not opts.only_index:
        try:
            os.unlink (opts.name)
        except:
            pass
    
    debug_print ("Processing Database")
    db = tabsqlitedb.tabsqlitedb ( filename = opts.name)
    #db.db.execute( 'PRAGMA synchronous = FULL; ' )
    
    def parse_source (f):
        _attri = []
        _table = []
        _gouci = []
        patt_com = re.compile(r'^###.*')
        patt_blank = re.compile(r'^[ \t]*$')
        patt_conf = re.compile(r'.*=.*')
        patt_table = re.compile(r'(.*)\t(.*)\t.*')
        patt_gouci = re.compile(r'.*\t.*')
        patt_s = re.compile(r'(.*)\t([\x00-\xff]{3})\t.*')

        for l in f:
            if ( not patt_com.match(l) ) and ( not patt_blank.match(l) ):
                for _patt, _list in ( (patt_table,_table),(patt_gouci,_gouci),(patt_conf,_attri) ):
                    if _patt.match(l):
                        _list.append(l)
                        break
        if not _gouci:
            #user didn't provide goucima, so we use the longest single character encode as the goucima.
            gouci_dict = {}
            for line in _table:
                res = patt_s.match(line)
                if res:
                    if gouci_dict.has_key(res.group(2)):
                        if len(res.group(1)) > len(gouci_dict[res.group(2)]):
                            gouci_dict[res.group(2)] = res.group(1)
                    else:
                        gouci_dict[res.group(2)] = res.group(1)
            for key in gouci_dict:
                _gouci.append('%s\t%s' %(key,gouci_dict[key] ) )
            _gouci.sort()

        return (_attri, _table, _gouci)

    def parse_pinyin (f):
        _pinyins = []
        patt_com = re.compile(r'^#.*')
        patt_blank = re.compile(r'^[ \t]*$')
        patt_py = re.compile(r'(.*)\t(.*)\t(.*)')
        patt_yin = re.compile(r'[a-z]+[1-5]')

        for l in f:
            if ( not patt_com.match(l) ) and ( not patt_blank.match(l) ):
                res = patt_py.match(l)
                if res:
                    yins = patt_yin.findall(res.group(2))
                    for yin in yins:
                        _pinyins.append("%s\t%s\t%s" \
                                % (res.group(1), yin, res.group(3)))
        return _pinyins[:]
    
    def parse_extra (f):
        _extra = []
        patt_com = re.compile(r'^###.*')
        patt_blank = re.compile(r'^[ \t]*$')
        patt_extra = re.compile(r'(.*)\t(.*)')
        patt_s = re.compile(r'(.*)\t([\x00-\xff]{3})\t.*')
        
        for l in f:
            if ( not patt_com.match(l) ) and ( not patt_blank.match(l) ):
                if patt_extra.match(l):
                    _extra.append(l)
        
        return _extra
    
    def pinyin_parser (f):
        for py in f:
            _zi, _pinyin, _freq = unicode (py,'utf-8').strip ().split()
            yield (_pinyin, _zi, _freq)

    def phrase_parser (f):
        list=[]
        for l in f:
            xingma, phrase, freq = unicode (l, "utf-8").strip ().split ('\t')[:3]
            list.append ( (xingma, phrase, int(freq), 0) )
        return list

    def goucima_parser (f):
        for l in f:
            zi,gcm = unicode (l, "utf-8").strip ().split ()
            yield (zi, gcm)
    
    def attribute_parser (f):
        for l in f:
            try:
                attr,val = unicode (l,"utf-8").strip().split ('=')
            except:
                attr,val = unicode (l,"utf-8").strip().split ('==')
            attr = attr.strip()
            origin_attr = attr
            attr = attr.lower()
            val = val.strip()
            yield (attr,val)
    
    def extra_parser (f):
        list = []
        for l in f:
            phrase, freq = unicode (l, "utf-8").strip ().split ()
            try:
                _tabkey = db.parse_phrase_to_tabkeys(phrase)
                list.append( (_tabkey,phrase,freq,0) )
            except:
                print '\"%s\" would not been added' % phrase.encode('utf-8')
        return list

    if opts.only_index:
        debug_print ('Only create Indexes')
        debug_print ( "Optimizing database " )
        db.optimize_database ()
    
        debug_print ('Create Indexes ')
        db.create_indexes ('main')
        debug_print ('Done! :D')
        return 0

    # now we parse the ime source file
    debug_print ("\tLoad sources \"%s\"" % opts.source)
    patt_s = re.compile( r'.*\.bz2' )
    _bz2s = patt_s.match(opts.source)
    if _bz2s:
        source = bz2.BZ2File ( opts.source, "r" )
    else:
        source = file ( opts.source, 'r' )
    # first get config line and table line and goucima line respectively
    debug_print ('\tParsing table source file ')
    attri,table,gouci =  parse_source ( source )
    
    debug_print ('\t  get attribute of IME :)')
    attributes = attribute_parser ( attri )
    debug_print ('\t  add attributes into DB ')
    db.update_ime ( attributes )
    db.create_tables ('main')

    # second, we use generators for database generating:
    debug_print ('\t  get phrases of IME :)')
    phrases = phrase_parser ( table)
    
    # now we add things into db
    debug_print ('\t  add phrases into DB ')
    db.add_phrases ( phrases )
    
    if db.get_ime_property ('user_can_define_phrase').lower() == u'true':
        debug_print ('\t  get goucima of IME :)')
        goucima = goucima_parser (gouci)
        debug_print ('\t  add goucima into DB ')
        db.add_goucima ( goucima )
    
    if db.get_ime_property ('pinyin_mode').lower() == u'true':
        debug_print ('\tLoad pinyin source \"%s\"' % opts.pinyin)
        _bz2p = patt_s.match(opts.pinyin)
        if _bz2p:
            pinyin_s = bz2.BZ2File ( opts.pinyin, "r" )
        else:
            pinyin_s = file ( opts.pinyin, 'r' )
        debug_print ('\tParsing pinyin source file ')
        pyline = parse_pinyin (pinyin_s)
        debug_print ('\tPreapring pinyin entries')
        pinyin = pinyin_parser (pyline)
        debug_print ('\t  add pinyin into DB ')
        db.add_pinyin ( pinyin )

    debug_print ("Optimizing database ")
    db.optimize_database ()
    
    if db.get_ime_property ('user_can_define_phrase').lower() == u'true' and opts.extra:
        debug_print( '\tPreparing for adding extra words' )
        db.create_indexes ('main')
        debug_print ('\tLoad extra words source \"%s\"' % opts.extra)
        _bz2p = patt_s.match(opts.extra)
        if _bz2p:
            extra_s = bz2.BZ2File ( opts.extra, "r" )
        else:
            extra_s = file ( opts.extra, 'r' )
        debug_print ('\tParsing extra words source file ')
        extraline = parse_extra (extra_s)
        debug_print ('\tPreparing extra words lines')
        db.cache_goucima()
        debug_print ('\t  Goucima has been cache to memory')
        extrawds = extra_parser (extraline)
        debug_print( '\t  we have %d extra phrases from source' % len(extrawds))
        # first get the entry of original phrases from
        # phrases-[(xingma, phrase, int(freq), 0)]
        orig_phrases = {}
        map (lambda x: orig_phrases.update({"%s\t%s"%(x[0],x[1]):x}), phrases )
        debug_print( '\t  the len of orig_phrases is: %d' % len(orig_phrases) )
        extra_phrases = {}
        map (lambda x: extra_phrases.update({"%s\t%s" %(x[0],x[1]):x}), extrawds )
        debug_print ( '\t  the len of extra_phrases is: %d' % len(extra_phrases) )
        # pop duplicated keys
        map (lambda x: extra_phrases.pop(x) if orig_phrases.has_key(x) else 0, extra_phrases.keys() )
        debug_print( '\t  %d extra phrases will be added' % len(extra_phrases))
        new_phrases = extra_phrases.values()
        debug_print ('\tAdding extra words into DB ')
        db.add_phrases (new_phrases)
        debug_print ("Optimizing database ")
        db.optimize_database ()
    
    if opts.index:
        debug_print ('Create Indexes ')
        db.create_indexes ('main')
    else:
        debug_print ("We don't create index on database, you should only active this function only for distribution purpose")
        db.drop_indexes ('main')
    debug_print ('Done! :D')
    
if __name__ == "__main__":
    main ()
