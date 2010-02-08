import wubi86
from OrderedSet import *
import sys

g_trans_map = {}
for comp in wubi86.g_quail_map:
    for i in range(1, len(comp)):
        prefix = comp[0:i]
        if prefix not in g_trans_map:
            g_trans_map[prefix] = OrderedSet()
        
        g_trans_map[prefix].add(comp[i])

trans_file = open("wubi86_trans.py", "w")
#trans_file = sys.stdout

trans_file.write("g_trans_map = {\n")

keys = list(g_trans_map.keys())
keys.sort()

for prefix in keys:
    trans = list(g_trans_map[prefix])
    trans.sort()
    trans = ''.join(trans)
    trans_file.write('%s : %s,\n' % (repr(prefix), repr(trans)))

trans_file.write("}\n")
