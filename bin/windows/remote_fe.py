#!/bin/env pycyg
import win32service
dood = [u'WinSta0']
for x in dood:
    try:
        handle = win32service.OpenWindowStation(x, True, 0)
        import find_exec
        find_exec.main('dood', 'explorer')
    except:
        print "exception in %s" % str(x)
        import traceback
        traceback.print_exc()
        pass
