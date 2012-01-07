#!/bin/env pycyg

from win32con import *
from win32api import *
import sys

if MessageBox(None, sys.argv[1], sys.argv[2], MB_YESNO) == IDYES:
    exit(0)
else:
    exit(1)

