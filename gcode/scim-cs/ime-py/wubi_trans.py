#!/usr/bin/python3

import sys, os
if 'SDIM_WUBI_FLAVOR' not in os.environ:
    os.environ['SDIM_WUBI_FLAVOR'] = 'wubi98_single'

wubi_prefix = os.environ['SDIM_WUBI_FLAVOR']
exec('import %s_trans as wubi_trans_table' % wubi_prefix)

g_trans_map = wubi_trans_table.g_trans_map
