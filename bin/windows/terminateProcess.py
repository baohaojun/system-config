#!/bin/env pycyg
from win32process import *
from win32con import *
from win32api import *
import os, sys, win32process

for x in sys.argv[1:]:
   print 'arg %s' % x
   try:
       TerminateProcess(OpenProcess(0x00010000L|0x1F0FFF, 0, int(x)), 0)
   except:
      import traceback
      traceback.print_exc()
