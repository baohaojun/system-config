#!/usr/bin/python3

import sys, os
import wubi

wubi_prefix = wubi.wubi_prefix
from OrderedSet import *

import regex as re

g_trans_map = {}
g_reverse_map = {}
char_re = re.compile("[a-z]")

for comp in wubi.g_quail_map:
    if char_re.match(comp):
        chars = [char for char in wubi.g_quail_map[comp] if len(char) == 1]
        for char in chars:
            if char not in g_reverse_map:
                g_reverse_map[char] = OrderedSet()

            g_reverse_map[char].add('"' + comp + '"')

    for i in range(1, len(comp)):
        prefix = comp[0:i]
        if prefix not in g_trans_map:
            g_trans_map[prefix] = OrderedSet()

        g_trans_map[prefix].add(comp[i])

trans_file = open(wubi_prefix + "_trans.py", "w")

reverse_file = open(wubi_prefix + "_reverse.py", "w")
#trans_file = sys.stdout

trans_file.write("g_trans_map = {\n")
reverse_file.write(
    "#!/bin/env python\n" +
    "# -*- coding: utf-8 -*-\n" +
    "g_reverse_map = {\n"
)

keys = list(g_trans_map.keys())
keys.sort()

for prefix in keys:
    trans = list(g_trans_map[prefix])
    trans.sort()
    trans = ''.join(trans)
    trans_file.write('%s : %s,\n' % (repr(prefix), repr(trans)))

trans_file.write("}\n")
trans_file.close()

keys = list(g_reverse_map.keys())
keys.sort()

for char in keys:
    reverse = list(g_reverse_map[char])
    reverse.sort()
    reverse = ', '.join(reverse)
    reverse_file.write('"%s" : (%s),\n' % (char, reverse))

reverse_file.write('}\n')
reverse_file.close()
