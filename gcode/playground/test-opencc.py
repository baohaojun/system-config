#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import opencc
import sys
import os

progname = os.path.basename(sys.argv[0])
if progname != 'test-opencc.py':
    cc = opencc.OpenCC(progname + '.json')
else:
    cc = opencc.OpenCC('s2j.json')

if len(sys.argv) == 1:
    print (cc.convert("亚"))
else:
    print (cc.convert(sys.argv[1]))
