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

app=QApplication(sys.argv)

app.setOrganizationName("Bao Haojun")
app.setOrganizationDomain("baohaojun.com")
app.setApplicationName("Imap4 mailbox monitor")

class ConfigDlg (QDialog):
    def __init__(self, parent=None):
        super(ConfigDlg, self).__init__(parent)
        settings = QSettings()
        layout = QGridLayout()
        
        self.host = settings.value("host", QVariant("bhj2")).toString()
        self.port = settings.value("port", QVariant("993")).toString()
        self.mailbox = settings.value("mailbox", QVariant("[Gmail].All Mail")).toString()
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

    def onExit(self):
        self.close()
        self.trayIcon.hide()

    def onOK(self):
        self.host = str(self.hostEdit.text())
        self.port = str(self.portEdit.text())
        self.mailbox = str(self.mailboxEdit.text())
        self.username = str(self.usernameEdit.text())

        settings = QSettings()
        settings.setValue("host", QVariant(self.host))
        settings.setValue("port", QVariant(self.port))
        settings.setValue("mailbox", QVariant(self.mailbox))
        settings.setValue("username", QVariant(self.username))
        self.hide()

        for auth in open(os.path.join(os.environ['HOME'], '/.authinfo')):
            authList = auth.split()
            if all((authList[1] == str(self.host),
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
            x=IMAP4_SSL(self.host, int(self.port))
            x.login(self.username, self.password)
            for mb in self.mailbox.split('/'):
                y = x.status(mb, '(unseen)')
                if y[0] != 'OK':
                    raise RuntimeError, 'IMAP result not OK'
                if '(UNSEEN 0)' not in y[1][0]:
                    self.trayIcon.setIcon(QIcon(":/got-mail.png"))
                    self.timer.start(2000)
                    break
            else:
                self.trayIcon.setIcon(QIcon(":/no-mail.png"))
                self.timer.start(300000)
        except:
            type_, value_ = sys.exc_info()[:2]
            self.trayIcon.showMessage("Error:", `type_` + ' ' + `value_`, QSystemTrayIcon.Information, 1)
            self.trayIcon.setIcon(QIcon(":/error-mail.png"))
            self.timer.start(300000)
        

    




        


config = ConfigDlg()
config.show()
app.exec_()

