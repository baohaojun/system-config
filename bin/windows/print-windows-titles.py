#!/bin/env pycyg
# -*- coding: gbk -*-
import win32gui, win32con, win32api
from win32process import *
from win32gui import *
from win32api import *
from win32con import *
import time, math, random, sys
import traceback
import os, re


g_hwndShell = GetDesktopWindow()

def getExecFromWnd(hwnd):
    threadId, procId = GetWindowThreadProcessId(hwnd)
    procHandle = None  
    try: 
        procHandle = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, False, procId)
        return GetModuleFileNameEx(procHandle, None).encode('gbk')
    except:
        return ''
    finally:
        if procHandle:
            CloseHandle(procHandle) 


def isWindowSwitchable2(hwnd):

    if not IsWindowVisible(hwnd):
        return False 

    global g_hwndShell

    dwStyle = GetWindowLong(hwnd, GWL_STYLE)
    hwndOwner=0
    hwndTmp = hwnd
    while(hwndTmp and hwndTmp != g_hwndShell):
        hwndOwner = hwndTmp
        hwndTmp = GetWindow(hwndTmp, GW_OWNER)

    dwStyleExOwner = GetWindowLong(hwndOwner, GWL_EXSTYLE)
    dwStyleExMe = GetWindowLong(hwnd, GWL_EXSTYLE)


    nTask = 0

    global listWindowInfo
    if hwnd != hwndOwner and dwStyleExMe & WS_EX_APPWINDOW:
        nTask = len(listWindowInfo)
    else:
        while nTask < len(listWindowInfo) and listWindowInfo[nTask]['owner'] != hwndOwner:
            nTask = nTask + 1;

    if nTask < len(listWindowInfo):
        pass
    else:
        sys.stdout.flush()
        if not (dwStyleExOwner & WS_EX_TOOLWINDOW) or dwStyleExMe & WS_EX_APPWINDOW \
        or (not (dwStyleExMe & WS_EX_TOOLWINDOW) and dwStyleExMe & WS_EX_CONTROLPARENT):
            if not (dwStyleExOwner & WS_EX_TOOLWINDOW) and dwStyleExMe & WS_EX_TOOLWINDOW:
                return False 
            
            wndInfo = {}
            wndInfo['hwnd'] = hwnd
            wndInfo['owner'] = hwndOwner
            wndInfo['class'] = GetClassName(hwnd)
            wndInfo['title'] = GetWindowText(hwnd)
            wndInfo['exec'] = getExecFromWnd(hwnd)
            listWindowInfo.append(wndInfo)



def isTopmost(hwnd):
    ex_style = GetWindowLong(hwnd, GWL_EXSTYLE)
    if ex_style&WS_EX_TOPMOST:
        return True
    return False

def _MyCallback(hwnd, extra):
    try:
        if IsWindowVisible(hwnd):
            print "0x%x \"%s\" \"%s\"" % (hwnd, GetClassName(hwnd), GetWindowText(hwnd));
            EnumChildWindows(hwnd, _MyCallback, None)
        sys.stdout.flush()
    except:
        sys.stdout.flush()
        traceback.print_exc()
        sys.stderr.flush()
    return True

listWindowInfo = []

execMap = {'msdev.exe' : 'msdev.exe|devenv.exe',
           'devenv.exe' : 'msdev.exe|devenv.exe',
           'dexplore.exe' : 'hh.exe|dexplore.exe',
           'hh.exe' : 'dexplore.exe',
           'alchemy.exe' : 'acrord32.exe|alchemy.exe|quill.exe',
           'quill.exe' : 'acrord32.exe|alchemy.exe|quill.exe',
           'acrord32.exe' : 'acrord32.exe|alchemy.exe|quill.exe',
           }

def ActivateWindow(hwnd):
    if IsIconic(hwnd):
        PostMessage(hwnd, WM_SYSCOMMAND, SC_RESTORE, 0)
        
    SetForegroundWindow(hwnd)

def cycleSameExecWnds():
    hwnd = GetForegroundWindow()
    exec_ = getExecFromWnd(hwnd)

    if not exec_:
        return

    exec_ = exec_.lower()
    exec_ = os.path.basename(exec_)
    if exec_ in execMap:
        exec_ = execMap[exec_]

    print 'exec_ is', exec_
    
    reobj = re.compile(exec_, re.I)
    global listWindowInfo
    listWindowInfo.reverse()
    for x in listWindowInfo:
        thisExec = x['exec'].lower()
        thisExec = os.path.basename(thisExec)
        if reobj.search(thisExec):
            ActivateWindow(x['hwnd'])
            break



def Usage():
    print 'Usage: find_exec.py match_ start_'
    print ''
    print '    Options: match_ the program to match'
    print '             start_ the program to start if match failed'

def debugWnd():
    for x in listWindowInfo:
        print "%x %x `%s' `%s' `%s'" % (x['hwnd'],
                                        x['owner'],
                                        x['title'],
                                        x['class'],
                                        x['exec'])

def main(*argv):
    EnumWindows(_MyCallback, (listWindowInfo,))
if __name__ == '__main__':
    sys.exit(main(*sys.argv))
