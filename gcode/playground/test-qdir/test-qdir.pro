#-------------------------------------------------
#
# Project created by QtCreator 2015-01-04T16:59:40
#
#-------------------------------------------------

QT       += core

QT       -= gui

TARGET = test-qdir
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app


SOURCES += main.cpp

HEADERS  += vcard.h

INCLUDEPATH += /usr/include/lua5.2
LIBS += -llua5.2
