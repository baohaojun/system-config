
#-------------------------------------------------
#
# Project created by QtCreator 2014-06-18T15:01:49
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = T1Wrench
TEMPLATE = app

unix:INCLUDEPATH += /usr/include/lua5.2
unix:LIBS += -llua5.2

SOURCES += main.cpp\
	t1wrenchmainwindow.cpp \
	adbstatethread.cpp \
        luaexecutethread.cpp \
        qcellphonetextedit.cpp \
        screencapture.cpp \
        painterwidget.cpp \
        painterrectitem.cpp \
        painterpathitem.cpp

HEADERS  += t1wrenchmainwindow.h \
	    adbstatethread.hpp \
            luaexecutethread.hpp \
            qcellphonetextedit.h \
            screencapture.h \
            painterwidget.h \
            painteritem.h \
            painterrectitem.h \
            painterpathitem.h

FORMS    += t1wrenchmainwindow.ui

RESOURCES += \
    T1Wrench.qrc
