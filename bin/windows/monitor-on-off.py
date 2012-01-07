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
import ctypes

def main(*argv):
    if argv[1] == "on":
        SendMessage(HWND_BROADCAST, WM_SYSCOMMAND, SC_MONITORPOWER, -1)
    elif (argv[1] == "off"):
        SendMessage(HWND_BROADCAST, WM_SYSCOMMAND, SC_MONITORPOWER, 2)
        ctypes.windll.user32.LockWorkStation()

    else:
        print "Usage: monitor-on-off.py on/off"
if __name__ == '__main__':
    sys.exit(main(*sys.argv))
