#!/bin/env pystart
from subprocess import *
import re
import sys

pipe = Popen('modules', stdout=PIPE).stdout
lines = pipe.read().split('\r\n')
reobj = re.compile('\(([0-9]+)\) : .*' + sys.argv[1], re.I)
print reobj


matches = []
for line in lines:
    matchObj = reobj.search(line)
    if matchObj:
        matches.append(matchObj.group(1))

import terminateProcess
terminateProcess.killProcesses(*matches)

