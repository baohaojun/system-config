#!/bin/env pycyg
from win32process import *
from win32con import *
from win32api import *
import os, sys, win32process

def killProcesses(*argv):
   for x in argv:
      print 'arg %s' % x
      try:
          TerminateProcess(OpenProcess(0x00010000L|0x1F0FFF, 0, int(x)), 0)
      except:
         import traceback
         traceback.print_exc()

if __name__ == '__main__':
    killProcesses(*sys.argv[1:])
