
#-------------------------------------------------
#
# Project created by QtCreator 2014-06-18T15:01:49
#
#-------------------------------------------------

include(qt-solutions/qtsingleapplication/src/qtsingleapplication.pri)

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = T1Wrench
TEMPLATE = app

win32 {
    RC_FILE = T1Wrench.rc
    INCLUDEPATH += ./windows
    LIBS += -L./windows -llua52
    CONFIG += console
}

unix {
  !macx {
    INCLUDEPATH += /usr/include/lua5.2
    LIBS += -llua5.2
  } else {
    INCLUDEPATH += ./macx
    LIBS += -L./macx -llua
    ICON = T1Wrench.icns
  }
}

SOURCES += main.cpp\
	t1wrenchmainwindow.cpp \
        adbstatethread.cpp \
        bhj_help.cpp \
        luaexecutethread.cpp \
        qcellphonetextedit.cpp \
        screencapture.cpp \
        painterwidget.cpp \
        painterrectitem.cpp \
        painterpathitem.cpp \
        emojimodel.cpp \
        filteringmodel.cpp \
        contactmodel.cpp \
        strlistmodel.cpp \
        filteringedit.cpp \
        dialoggetentry.cpp \
        filteringlistview.cpp \
    fileopenfilter.cpp

HEADERS  += t1wrenchmainwindow.h \
	    adbstatethread.hpp \
            luaexecutethread.hpp \
            qcellphonetextedit.h \
            screencapture.h \
            painterwidget.h \
            painteritem.h \
            painterrectitem.h \
            painterpathitem.h \
            emojimodel.h \
            filteringmodel.h \
            contactmodel.h \
            strlistmodel.h \
            t1wrench.h \
            filteringedit.h \
            filteringlistview.h \
            filteringmodel.h \
            dialoggetentry.h \ 
    fileopenfilter.h

FORMS    += t1wrenchmainwindow.ui \
            dialoggetentry.ui

RESOURCES += \
    T1Wrench.qrc
