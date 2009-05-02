#!/q/bin/pycyg.exe -x
# -*- coding: gbk -*-
from win32api import *
from win32con import *

import sys, os, traceback, re

rootKeysMap = {HKEY_CLASSES_ROOT: "HKEY_CLASSES_ROOT",
               HKEY_CURRENT_USER: "HKEY_CURRENT_USER",
               HKEY_LOCAL_MACHINE: "HKEY_LOCAL_MACHINE",
               HKEY_USERS: "HKEY_USERS",
               }

if len(sys.argv) != 2:
    print 'Usage: regsub.py pattern'
    sys.exit(-1)

reobj = re.compile(sys.argv[1], re.I)


def traverseKey(rootKey, rootKeyName):
        
    #replace the values of the rootKey first

    ## get all the values
    allValues = []
    try:
        index = 0
        while True: 
            (valueName, value, type_) = RegEnumValue(rootKey, index)
            index = index+1
            allValues.append((valueName, value, type_))
    except: #this will always occur when there are no more values in the key
        sys.stdout.flush()

    ## handle the values according to their value / valueName / type
    
    for (valueName, value, type_) in allValues:
        ### if value contains a '?', then probably not a Unicode char not encodable by our coding (gbk)
        if type_ in (REG_SZ, REG_EXPAND_SZ) and '?' in value:
            sys.stderr.write('key %s\nname %s\ntype %s\nvalue %s\n\n' % (rootKeyName, valueName, `type_`, value))
            continue

        if type_ == REG_MULTI_SZ and any([('?' in value) for x in value]):
            sys.stderr.write('key %s\nname %s\ntype %s\nvalue %s\n\n' % (rootKeyName, valueName, `type_`, `value`))
            continue
        
        needReplaceName = False 
        needReplaceValue = False

        ### if we need sub the name?
        if reobj.search(valueName):
            needReplaceName = True

        ### do we need sub the value string?
        if type_ in (REG_SZ, REG_EXPAND_SZ) and reobj.search(value):
            needReplaceValue = True
            
        ### do we need sub the strings in value?
        if type_ == REG_MULTI_SZ and any([reobj.search(x) for x in value]):
            needReplaceValue = True 

        ### load the new value
        if needReplaceValue or needReplaceName:
                sys.stdout.write('matched [%s]: "%s"="%s"\n' % (rootKeyName, valueName, value))

    #now do the tree walking
    try:
        subKeys = RegEnumKeyEx(rootKey) #get all the keys
    except:
        sys.stderr.write("Exception: RegEnumKeyEx %s\n" % rootKeyName)
    else:
        for subK in subKeys:
            try:
                leafKey = RegOpenKeyEx(rootKey, subK[0], 0, KEY_ALL_ACCESS)
            except:
                sys.stderr.write("Exception: RegOpenKeyEx %s\n" % (rootKeyName+'\\'+subK[0]))
            else:
                traverseKey(leafKey, "%s\\%s" % (rootKeyName, subK[0]))

        

                
        
for rootKey in rootKeysMap:
    #rootKey need to be opened with all access
    rootKeyName = rootKeysMap[rootKey]
    try:
        rootKey = RegOpenKeyEx(rootKey, None, 0, KEY_ALL_ACCESS)
    except:
        sys.stderr.write('Error openning %s\n' % rootKeysMap[rootKey])
        continue
    else:
        traverseKey(rootKey, rootKeyName)

sys.stderr.write('\n')
sys.stderr.write('Every thing has been successively dumped!\n')
