#!/usr/bin/python3

import sys, os
if 'SDIM_WUBI_FLAVOR' not in os.environ:
    os.environ['SDIM_WUBI_FLAVOR'] = 'wubi98_single'

wubi_prefix = os.environ['SDIM_WUBI_FLAVOR']
exec('import %s_reverse as wubi_reverse_table' % wubi_prefix)

g_reverse_map = wubi_reverse_table.g_reverse_map
