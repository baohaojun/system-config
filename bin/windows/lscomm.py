#!/bin/env pycyg
import win32api
import win32con
def getAvailablePorts ():
    """"""
    ports = []
    try:
        key = win32api.RegOpenKeyEx(win32con.HKEY_LOCAL_MACHINE, r'hardware\devicemap\serialcomm')
        i=0
        while True:
            try:
                port = win32api.RegEnumValue(key, i)[1]
                ports.append(port)
                i+=1
            except: #reached the last value
                break
    except: #this system does not have the registry entry?
        pass
    if not ports: #any way, let's fake it.
        return ['com'+str(x+1) for x in range(16)]
    return ports

for x in getAvailablePorts():
    print x
