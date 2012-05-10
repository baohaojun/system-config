import wubi86
from OrderedSet import *
import sys

g_single_map = {}
for comp in wubi86.g_quail_map:
    g_single_map[comp] = [x for x in filter(lambda y: len(y) == 1, wubi86.g_quail_map[comp])]

single_file = open("wubi86_single.py", "w")
#single_file = sys.stdout

single_file.write("""#!/bin/env python
# -*- coding: utf-8 -*-

g_quail_map = {
""")


comps = list(g_single_map.keys())
comps.sort()

for comp in comps:
    singles = list(g_single_map[comp])
    single_file.write('%s : %s,\n' % (repr(comp), repr(singles)))

single_file.write("}\n")
