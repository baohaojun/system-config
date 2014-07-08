#-------------------------------------------------
#
# Project created by QtCreator 2014-06-18T15:01:49
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = T1Wrench
TEMPLATE = app


SOURCES += main.cpp\
	t1wrenchmainwindow.cpp \
	adbstatethread.cpp \
	readinfothread.cpp \
    promptdialog.cpp

HEADERS  += t1wrenchmainwindow.h \
	    adbstatethread.hpp \
	    readinfothread.hpp \
    promptdialog.h

FORMS    += t1wrenchmainwindow.ui \
    promptdialog.ui

RESOURCES += \
    T1Wrench.qrc
