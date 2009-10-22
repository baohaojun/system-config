#!/bin/env pycyg
import win32gui
HWND_BROADCAST      = 0xFFFF
WM_SETTINGCHANGE    = 0x001A
SMTO_ABORTIFHUNG    = 0x0002
sParam              = "Environment"

import win32gui
res1, res2          = win32gui.SendMessageTimeout(HWND_BROADCAST,
                        WM_SETTINGCHANGE, 0, sParam, SMTO_ABORTIFHUNG, 100)
if  not res1:
    print ("result %s, %s from SendMessageTimeout" % (bool(res1), res2))
