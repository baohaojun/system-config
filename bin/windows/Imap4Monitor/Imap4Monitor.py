#!/bin/env pywstart

import sys, subprocess, stat, os, traceback, functools
from qrc_resources import *

import struct
import thread
from random import *
from PyQt4.QtCore import *
from PyQt4.QtGui import *
import threading
from imaplib import *

_long_check_period = 60 * 60 * 1000

app=QApplication(sys.argv)

app.setOrganizationName("Bao Haojun")
app.setOrganizationDomain("baohaojun.com")
app.setApplicationName("Imap4 mailbox monitor")

class ConfigDlg (QDialog):
    def __init__(self, parent=None):
        super(ConfigDlg, self).__init__(parent)
        settings = QSettings()
        layout = QGridLayout()

        self.last_check_has_new_mail = False
        
        self.host = settings.value("host", QVariant("localhost")).toString()
        self.port = settings.value("port", QVariant("0")).toString()
        self.port = int(self.port)
        self.ssl = False
        if self.port == 993:
            self.ssl = True
        self.mailbox = settings.value("mailbox", QVariant("Inbox")).toString()
        self.username = settings.value("username", QVariant("bhj")).toString()

        label = QLabel("Host:")
        self.hostEdit = QLineEdit(self.host)
        layout.addWidget(label, 0, 0)
        layout.addWidget(self.hostEdit, 0, 1)


        label = QLabel("Port:")
        self.portEdit = QLineEdit(str(self.port))
        layout.addWidget(label, 1, 0)
        layout.addWidget(self.portEdit, 1, 1)

        label = QLabel("Mailbox:")
        self.mailboxEdit = QLineEdit(self.mailbox)
        layout.addWidget(label, 2, 0)
        layout.addWidget(self.mailboxEdit, 2, 1)

        label = QLabel("Username:")
        self.usernameEdit = QLineEdit(self.username)
        layout.addWidget(label, 3, 0)
        layout.addWidget(self.usernameEdit, 3, 1)

        self.okButton= QPushButton("OK")
        self.cancelButton = QPushButton("Cancel")
        layoutBot = QHBoxLayout()
        layoutBot.addWidget(self.okButton)
        layoutBot.addWidget(self.cancelButton)

        layout.addLayout(layoutBot, 4, 0, 1, 2)
        self.setLayout(layout)        

        self.trayIcon = QSystemTrayIcon(QIcon(":/no-mail.png"))
        self.trayIcon.show()
        
        self.exitAction = QAction("Exit", None)
        self.connect(self.exitAction, SIGNAL("triggered()"), self.onExit)

        self.checkAction = QAction("Check again", None)
        self.connect(self.checkAction, SIGNAL("triggered()"), self.checkMail)

        self.trayMenu = QMenu()
        self.trayMenu.addAction(self.checkAction)
        self.trayMenu.addAction(self.exitAction)
        self.trayIcon.setContextMenu(self.trayMenu)

        self.connect(self.okButton, SIGNAL("clicked()"), self.onOK)
        self.connect(self.cancelButton, SIGNAL("clicked()"), self.onExit)
        self.timer = QTimer(self)
        self.timer.setSingleShot(True)
        self.connect(self.timer, SIGNAL("timeout()"), self.onOK)
        self.timer.start(10)


    def onExit(self):
        self.close()
        self.trayIcon.hide()

    def onOK(self):
        self.host = str(self.hostEdit.text())
        self.port = str(self.portEdit.text())
        self.port = int(self.port)
        self.mailbox = str(self.mailboxEdit.text())
        self.username = str(self.usernameEdit.text())

        settings = QSettings()
        settings.setValue("host", QVariant(self.host))
        settings.setValue("port", QVariant(self.port))
        settings.setValue("mailbox", QVariant(self.mailbox))
        settings.setValue("username", QVariant(self.username))
        self.hide()

        for auth in open(os.path.join(os.environ['HOME'], '.authinfo')):
            authList = auth.split()
            if len(authList) > 7 and all((authList[1] == str(self.host),
                   authList[3] == str(self.username),
                   authList[7] == str(self.port))):
                self.password = authList[5]
                break

        self.timer = QTimer(self)
        self.timer.setSingleShot(True)
        self.connect(self.timer, SIGNAL("timeout()"), self.checkMail)
        self.checkMail()

    def checkMail(self):
        try:
            result = os.system("bash has-new-mail.sh") / 256
            if result == 0:
                self.trayIcon.setIcon(QIcon(":/got-mail.png"))
                if not self.last_check_has_new_mail:
                    self.trayIcon.showMessage("Hey!", "You've got mail", 5000)
                self.last_check_has_new_mail = True
                self.timer.start(5000)
            else:
                self.trayIcon.setIcon(QIcon(":/no-mail.png"))
                self.last_check_has_new_mail = False
                next_check = _long_check_period
                if result == 2:
                    next_check = 5000
                self.timer.start(next_check)
        except:
            pass

config = ConfigDlg()
app.exec_()

