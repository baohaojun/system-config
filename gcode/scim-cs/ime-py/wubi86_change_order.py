#!/usr/bin/python
# -*- coding: utf-8 -*-

import wubi86
import re
import sys

g_freq_map = {}

freq_file = open("CharFreq.txt")
while True:
    line = freq_file.readline()
    if not line:
        break
    split = line.split()
    if len(split) < 3:
        continue
    seq, char, times = split[0], split[1], split[2]
    if not re.match(r"^\d+$", seq + times):
        continue

    g_freq_map[char] = int(times)


import os

def format_tuple(t):
    return '(' + ' '.join(['"' + x + '",' for x in t]) + "),\n"

def sort_freq(a, b):
    if a not in g_freq_map and b not in g_freq_map:
        return 0
    if a not in g_freq_map:
        return -1
    if b not in g_freq_map:
        return 1
    if g_freq_map[a] < g_freq_map[b]:
        return -1
    elif g_freq_map[a] == g_freq_map[b]:
        return 0
    else:
        return 1

def print_head():
    sys.stdout.write("""#!/bin/env python
# -*- coding: utf-8 -*-

g_quail_map = {
""")

def print_code_words(code, words):
    if re.search('"', code):
        code = repr(code)
    else:
        code = '"%s"' % code

    sys.stdout.write(code + ' : ' + format_tuple(words))


if re.search("wubi86_change_order.py", sys.argv[0]):
    keys = list(wubi86.g_quail_map.keys())
    keys.sort()

    print_head()
    for code in keys:
        words = wubi86.g_quail_map[code]
        need_fix = False
        for current in range(0, len(words)):
            if need_fix:
                break
            current_char = words[current]

            for next in range(current, len(words)):
                next_char = words[next]
                if next_char in g_freq_map and not current_char in g_freq_map:
                    need_fix = True
                    break
                if next_char not in g_freq_map or current_char not in g_freq_map:
                    continue
                if g_freq_map[current_char] < g_freq_map[next_char]:
                    need_fix = True
                    break

        if need_fix:
            words = list(words)
            words.sort(sort_freq)
            words.reverse()
            words = tuple(words)

        print_code_words(code, words)

    sys.stdout.write("}\n")
elif re.search("wubi86_detect_multi.py", sys.argv[0]):
    g_hanzi_code_map = {}

    for code in wubi86.g_quail_map.keys():
        if code[0] == 'z':
            continue
        words = wubi86.g_quail_map[code]
        for hanzi in words:
            if hanzi in g_hanzi_code_map:
                g_hanzi_code_map[hanzi].append(code)
            else:
                g_hanzi_code_map[hanzi] = [code]

    hanzis = list(g_hanzi_code_map.keys())
    hanzis.sort()

    for hanzi in g_hanzi_code_map.keys():
        if len(g_hanzi_code_map[hanzi]) > 1 and hanzi in g_freq_map and g_freq_map[hanzi] >= 1:
            sys.stdout.write(hanzi + " " + ' '.join(g_hanzi_code_map[hanzi]) + "\n")

elif re.search("wubi86_delete_code.py", sys.argv[0]):
    keys = list(wubi86.g_quail_map.keys())
    keys.sort()

    print_head()

    for code in keys:
        if code == sys.argv[1]:
            words = wubi86.g_quail_map[code]
            words = [x for x in words if x != sys.argv[2]]
            words = tuple(words)
            wubi86.g_quail_map[code] = words
        print_code_words(code, wubi86.g_quail_map[code])

    sys.stdout.write("}\n")
