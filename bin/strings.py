#!/bin/python
from curses.ascii import *
import sys

bytes = open(sys.argv[1]).read()

for x in bytes:
    if isprint(x):
        sys.stdout.write(x)
