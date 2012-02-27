#!/usr/bin/env python

import unicodedata
import sys
while True:
    line = sys.stdin.readline()
    if not line:
        break
    if line[-1] == "\n":
        line = line[:-1]
    print unicodedata.normalize('NFKD', line.decode('utf-8')).encode('ascii','ignore')
