#!/bin/env pystart
from subprocess import *
import re
import sys

pipe = Popen('modules', stdout=PIPE).stdout
lines = pipe.read().split('\r\n')
reobj = re.compile('\(([0-9]+)\) : .*' + sys.argv[1], re.I)
print reobj


import os
my_pid = os.getpid()

matches = []
for line in lines:
    matchObj = reobj.search(line)
    if not matchObj:
        continue
    if my_pid == int(matchObj.group(1)): #don't kill myself
        continue
    if matchObj.group(1) in matches: #don't kill twice
        continue

    matches.append(matchObj.group(1))

import terminateProcess
terminateProcess.killProcesses(*matches)
