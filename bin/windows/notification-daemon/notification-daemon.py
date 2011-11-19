#!/bin/env pywstart

from socket import *
import sys, subprocess, stat, os, traceback, functools
from qrc_resources import *

import struct
import thread
from random import *
from PyQt4.QtCore import *
from PyQt4.QtGui import *
import threading
from imaplib import *

app=QApplication(sys.argv)

app.setOrganizationName("Bao Haojun")
app.setOrganizationDomain("baohaojun.com")
app.setApplicationName("notification-daemon")

ime_listen_sock = socket(AF_INET, SOCK_STREAM);

if os.name == 'posix':
    ime_listen_sock.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)

ime_listen_sock.bind(('127.0.0.1', 23456))
ime_listen_sock.listen(5)

class ThreadNotificationReceiver(QThread):
    """ """
    def __init__ (self, parent=None):
        """ """
        super(ThreadNotificationReceiver, self).__init__(parent)
    def run (self):
        while True:
            readsock = ime_listen_sock.accept()[0]
            readsock.shutdown(SHUT_WR)
            readsock = readsock.makefile("rwb", 0)
            try:
                title = readsock.readline().decode('utf-8')
                content = readsock.read().decode('utf-8')
                self.emit(SIGNAL("receivedSth(QString, QString)"), title, content)
            except:
                pass
            finally:
                readsock.close()

class ConfigDlg (QDialog):
    def __init__(self, parent=None):
        super(ConfigDlg, self).__init__(parent)

        self.trayIcon = QSystemTrayIcon(QIcon(":/notifier.png"))
        
        self.clearAction = QAction("Clear all", None)
        self.connect(self.clearAction, SIGNAL("triggered()"), self.onClear)

        self.trayMenu = QMenu()
        self.trayMenu.addAction(self.clearAction)
        self.trayIcon.setContextMenu(self.trayMenu)

        traySignal = "activated(QSystemTrayIcon::ActivationReason)"
        self.connect(self.trayIcon, SIGNAL(traySignal), self.__icon_activated)

        self.receivedNotes = []
        self.receiverThread = ThreadNotificationReceiver()
        self.connect(self.receiverThread, SIGNAL("receivedSth(QString, QString)"), self.onReceivedSth)
        self.receiverThread.start()

    def __icon_activated(self, reason):
        if reason == QSystemTrayIcon.DoubleClick:
            self.receivedNotes = []
            self.trayIcon.hide()
        if reason == QSystemTrayIcon.Trigger:
            if self.receivedNotes:
                title = self.receivedNotes[-1][0]
                content = self.receivedNotes[-1][1]
                self.trayIcon.showMessage(title, content, 5000)
            else:
                self.trayIcon.hide()
                
        

    def onReceivedSth(self, title, content):
        # self.clearAction = QAction("Clear all", None)
        # self.connect(self.clearAction, SIGNAL("triggered()"), self.onClear)
        # self.trayMenu = QMenu()
        # self.trayMenu.addAction(self.clearAction)
        # self.trayIcon.setContextMenu(self.trayMenu)

        action = QAction(title, None)
        self.connect(action, SIGNAL("triggered()"), self.makeAction(action, title, content))
        self.trayMenu.addAction(action)

        self.receivedNotes.append((title, content))
        self.trayIcon.show()
        self.trayIcon.showMessage(title, content, 5000)

    def makeAction(self, action, title, content):
        def doAction():
            self.trayIcon.showMessage(title, content, 5000)
            self.trayMenu.removeAction(action)
            action.deleteLater() # no need for disconnect?
        return doAction
            

    def onClear(self):
        self.trayIcon.hide()

    def onOK(self):
        self.hide()

config = ConfigDlg()
app.exec_()

