#!/bin/env pywstart
import win32com.client
from socket import *
import sys

msdn = win32com.client.Dispatch('DExplore.AppObj.9.0')
msdn.SetCollection('ms-help://MS.VSCC.v90/', '')
sockListening = socket(AF_INET, SOCK_STREAM)
sockListening.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
sockListening.bind(('', 3836))
sockListening.listen(5)
while True:
    sockAskMsdn, addr = sockListening.accept()
    line = sockAskMsdn.recv(1024)
    sockAskMsdn.close()
    sys.stdout.flush()
    try:
        msdn.DisplayTopicFromF1Keyword(line)
    except:
        pass
