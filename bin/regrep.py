from win32api import *
from win32con import *

import sys, os, traceback, re

rootKeysMap = {HKEY_CLASSES_ROOT: "HKEY_CLASSES_ROOT",
               HKEY_CURRENT_USER: "HKEY_CURRENT_USER",
               HKEY_LOCAL_MACHINE: "HKEY_LOCAL_MACHINE",
               HKEY_USERS: "HKEY_USERS",
               }


reobj = re.compile(sys.argv[1], re.I)
print 'Windows Registry Editor Version 5.00'

lastPrintKeyName = ""
currentKeyName = ""

fileIndex = 0
File = None
def printValue(valueName, value):
    global lastPrintKeyName
    global currentKeyName
    global fileIndex
    global File
    if lastPrintKeyName != currentKeyName:
        if File:
            File.close()
        File = open(str(fileIndex)+".reg", "w")
        fileIndex = fileIndex+1
        lastPrintKeyName = currentKeyName
        File.write('Windows Registry Editor Version 5.00\n')
        File.write("\n")
        File.write('[%s]\n' % currentKeyName)

    valueName = r'\\'.join(valueName.split('\\')) #replace '\\' with r'\\'
    valueName = r'\"'.join(valueName.split('"')) #replace '"' with r'\"'

    value = r'\\'.join(value.split('\\'))
    value = r'\"'.join(value.split('"'))

    if valueName:
        File.write('"%s"="%s"\n' % (valueName, value))
    else:
        File.write('@="%s"\n' % value)
        
    File.flush()
                           
def traverseKey(key, keyName):
    global currentKeyName
    currentKeyName = keyName
    if reobj.search(keyName):
        print
        print '[%s]' % keyName
       
        
    try:
        index = 0
        while True: 
            (valueName, value, type_) = RegEnumValue(key, index)
            index = index+1
            if type_ == 1: # 1 is REG_SZ, we only want to handle this type
                if reobj.search(valueName):
                    printValue(valueName, value)
                    continue #no need to print it twice if the value also matches
                if reobj.search(value):
                    printValue(valueName, value)

    except:
        sys.stdout.flush()

    try:
        leafKeys = RegEnumKeyEx(key)
    except:
        sys.stderr.write("Exception: RegEnumKeyEx %s\n" % keyName)
    else:
        for leaf in leafKeys:
            try:
                leafKey = RegOpenKeyEx(key, leaf[0])
                traverseKey(leafKey, "%s\\%s" % (keyName, leaf[0]))
            except:
                sys.stderr.write("Exception: RegOpenKeyEx %s\n" % (keyName+'\\'+leaf[0]))
        

                
        
for x in rootKeysMap:
    traverseKey(x, rootKeysMap[x])

sys.stderr.write('\n')
sys.stderr.write('Every thing has been successively dumped!\n')
