
#-------------------------------------------------
#
# Project created by QtCreator 2014-06-18T15:01:49
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = T1Wrench
TEMPLATE = app

win32:INCLUDEPATH += ./windows
win32:LIBS += -L./windows -llua52
win32:CONFIG += console

unix {
  !macx {
    INCLUDEPATH += /usr/include/lua5.2
    LIBS += -llua5.2
  } else {
    INCLUDEPATH += ./macx
    LIBS += -L./macx -llua
  }
}

SOURCES += main.cpp\
	t1wrenchmainwindow.cpp \
	adbstatethread.cpp \
        luaexecutethread.cpp \
        qcellphonetextedit.cpp \
        screencapture.cpp \
        painterwidget.cpp \
        painterrectitem.cpp \
        painterpathitem.cpp \
    dialoggetemoji.cpp \
    emojimodel.cpp \
    emojifilteredit.cpp \
    emojilistview.cpp

HEADERS  += t1wrenchmainwindow.h \
	    adbstatethread.hpp \
            luaexecutethread.hpp \
            qcellphonetextedit.h \
            screencapture.h \
            painterwidget.h \
            painteritem.h \
            painterrectitem.h \
            painterpathitem.h \
            dialoggetemoji.h \
            emojimodel.h \
            t1wrench.h \
    emojifilteredit.h \
    emojilistview.h

FORMS    += t1wrenchmainwindow.ui \
    dialoggetemoji.ui

RESOURCES += \
    T1Wrench.qrc
