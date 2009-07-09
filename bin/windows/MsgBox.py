#!/bin/env pycyg

from win32con import *
from win32api import *
import sys

MessageBox(None, sys.argv[1], sys.argv[2], MB_YESNO)
