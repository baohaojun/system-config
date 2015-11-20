QT       += core gui
greaterThan(QT_MAJOR_VERSION, 4): QT += widgets
TEMPLATE = app
TARGET = vnc-qtonly
DEPENDPATH += .
INCLUDEPATH += .
LIBS += -lvncclient -lgnutls
DEFINES += QTONLY

HEADERS += remoteview.h vncclientthread.h vncview.h
SOURCES += main.cpp remoteview.cpp vncclientthread.cpp vncview.cpp
