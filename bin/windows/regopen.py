#!/bin/env pycyg
# -*- coding: gbk -*-
from win32api import *
from win32con import *
import terminateWindow

import sys, os, traceback, re, subprocess

rootKeysMap = {HKEY_CLASSES_ROOT: "HKEY_CLASSES_ROOT",
               HKEY_CURRENT_USER: "HKEY_CURRENT_USER",
               HKEY_LOCAL_MACHINE: "HKEY_LOCAL_MACHINE",
               HKEY_USERS: "HKEY_USERS",
               }

if len(sys.argv) != 2:
    print 'Usage: regopen.py reg_key'
    sys.exit(-1)



keyToOpen = sys.argv[1]
if keyToOpen[0:5] == '/proc': #/proc/registry
    keyToOpen = keyToOpen[len('/proc/registry/'):]
    reobj = re.compile('/')
    keyToOpen = reobj.sub('\\\\', keyToOpen)

    if '%' in keyToOpen:
        keyToOpenSub = keyToOpen
        keyToOpen = ''
        while '%' in keyToOpenSub:
            i = keyToOpenSub.find('%')
            hexStr = '0x'+keyToOpenSub[i+1:i+3]
            print 'hexStr is', hexStr
            hexVal = int(hexStr, 16)
            keyToOpen = keyToOpen + keyToOpenSub[0:i] + chr(hexVal)
            keyToOpenSub = keyToOpenSub[i+3:]
        keyToOpen = keyToOpen + keyToOpenSub
keyToOpen = 'My Computer\\'+keyToOpen
print 'keyToOpen is', keyToOpen

regEditKey = RegOpenKeyEx(HKEY_CURRENT_USER, 
                       'Software\\Microsoft\\Windows\\CurrentVersion\\Applets\\Regedit', 
                       0, 
                       KEY_ALL_ACCESS)
favoriteKey = RegOpenKeyEx(regEditKey, 'Favorites', 0, KEY_ALL_ACCESS)
try:
    while terminateWindow.terminateWindow(['regedit.exe']):
        pass
except:
    traceback.print_exc()

RegSetValueEx(favoriteKey, "from regopen.py", 0, REG_SZ, keyToOpen)
RegSetValueEx(regEditKey, "LastKey", 0, REG_SZ, keyToOpen)
subprocess.Popen(('regedit.exe',))
