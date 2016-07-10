
#-------------------------------------------------
#
# Project created by QtCreator 2014-06-18T15:01:49
#
#-------------------------------------------------

include(qt-solutions/qtsingleapplication/src/qtsingleapplication.pri)

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = Wrench
TEMPLATE = app

win32 {
    RC_FILE = Wrench.rc
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
    ICON = Wrench.icns
  }
}

SOURCES += main.cpp\
        adbclient.cpp \
	wrenchmainwindow.cpp \
        adbstatethread.cpp \
        adbphonescreenthread.cpp \
        bhj_help.cpp \
        luaexecutethread.cpp \
        qcellphonetextedit.cpp \
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
    fileopenfilter.cpp \
    phonescreendialog.cpp \
    phonescreensyncer.cpp

HEADERS  += wrenchmainwindow.h \
            adbclient.h \
	    adbstatethread.hpp \
            adbphonescreenthread.hpp \
            luaexecutethread.hpp \
            qcellphonetextedit.h \
            painterwidget.h \
            painteritem.h \
            painterrectitem.h \
            painterpathitem.h \
            emojimodel.h \
            filteringmodel.h \
            contactmodel.h \
            strlistmodel.h \
            wrench.h \
            filteringedit.h \
            filteringlistview.h \
            filteringmodel.h \
            dialoggetentry.h \
    fileopenfilter.h \
    phonescreendialog.h \
    phonescreensyncer.h

FORMS    += wrenchmainwindow.ui \
            dialoggetentry.ui \
    phonescreendialog.ui

RESOURCES += \
    Wrench.qrc
