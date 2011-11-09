#!/bin/env pycyg
# -*- coding: gbk -*-
import win32gui, win32con, win32api
from win32process import *
from win32gui import *
from win32api import *
from win32con import *
import time, math, random, sys
import traceback
import os, re
import win32con

def main(*argv):
    argv1 = argv[1].upper()
    argv2 = argv[2].upper()
    assert argv1 in dir(win32con)
    assert argv2 in dir(win32con)
    for x in argv[3:]:
        PostMessage(int(x, 0), eval(argv1), eval(argv2), 0)
if __name__ == '__main__':
    sys.exit(main(*sys.argv))
