from win32api import *
from win32con import *

import sys, os, traceback, re

rootKeysMap = {HKEY_CLASSES_ROOT: "HKEY_CLASSES_ROOT",
               HKEY_CURRENT_USER: "HKEY_CURRENT_USER",
               HKEY_LOCAL_MACHINE: "HKEY_LOCAL_MACHINE",
               HKEY_USERS: "HKEY_USERS",
               }

reobj = re.compile("documents and settings", re.I)

def traverseKey(key, keyName):
    if reobj.search(keyName):
        print 'keyName:', keyName
        
    try:
        index = 0
        while True:
            (valueName, value, type_) = RegEnumValue(key, index)
            index = index+1
            if type(valueName) is str and reobj.search(valueName):
                print 'valueName:', valueName

            if type(value) is str and reobj.search(value):
                print '%s\\%s : %s' % (keyName, valueName, value)

    except:
        sys.stdout.flush()

    try:
        leafKeys = RegEnumKeyEx(key)
        for leaf in leafKeys:
            leafKey = RegOpenKeyEx(key, leaf[0])
            traverseKey(leafKey, "%s\\%s" % (keyName, leaf[0]))
    except:
        pass
                
        
for x in rootKeysMap:
    traverseKey(x, rootKeysMap[x])



