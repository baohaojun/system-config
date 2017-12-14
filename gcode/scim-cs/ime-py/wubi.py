#!/usr/bin/python3

import sys, os
if 'SDIM_WUBI_FLAVOR' not in os.environ:
    os.environ['SDIM_WUBI_FLAVOR'] = 'wubi98_single'

wubi_prefix = os.environ['SDIM_WUBI_FLAVOR']
exec('import %s as wubi_table' % wubi_prefix)

import wubi_extra

g_quail_map = wubi_table.g_quail_map
for key in wubi_extra.g_quail_map.keys():
    g_quail_map[key] = wubi_extra.g_quail_map[key]
