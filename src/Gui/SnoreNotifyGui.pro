# -------------------------------------------------
# Project created by QtCreator 2009-11-24T23:39:55
# -------------------------------------------------
QT += dbus
QT += gui
QT += network
TARGET = SnoreNotifyGui
TEMPLATE = app
INCLUDEPATH += ../. ./
HEADERS += mainwindow.h \
    ../dbusbinding.h \
    ../application.h
FORMS += mainwindow.ui
SOURCES += ../dbusbinding.cpp \
    ../application.cpp \
    gmain.cpp\
     mainwindow.cpp
