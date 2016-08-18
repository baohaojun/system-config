
#-------------------------------------------------
#
# Project created by QtCreator 2014-06-18T15:01:49
#
#-------------------------------------------------

include(qt-solutions/qtsingleapplication/src/qtsingleapplication.pri)

QT       += core gui network opengl

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

greaterThan(QT_MAJOR_VERSION, 4) {
    QT += widgets testlib} else {
    CONFIG += qtestlib
}

greaterThan(DEBUG, 0) {
    DEFINES += QVNCVIEWER_DEBUG
    CONFIG += warn_on debug
} else {
    DEFINES += QVNCVIEWER_RELEASE
    CONFIG += warn_off release
}


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

INCLUDEPATH += common libjpeg rfb zlib

unix {
    isEmpty(INSTALL_BIN_PATH) {
        target.path = /usr/local/bin
    } else {
        target.path = $$INSTALL_BIN_PATH
    }
    INSTALLS += target
    LIBS += -lgcrypt
}

win32 {
    LIBS += -lws2_32
}


SOURCES += main.cpp\
        adbclient.cpp \
	wrenchmainwindow.cpp \
        adbstatethread.cpp \
        adbvncthread.cpp \
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
    phonescreensyncer.cpp \
    mainwindow.cpp \
    connectionwindow.cpp \
    common/d3des.c \
    common/md5.c \
    common/minilzo.c \
    common/sha1.c \
    common/turbojpeg.c \
    common/vncauth.c \
    libvncclient/cursor.c \
    libvncclient/h264.c \
    libvncclient/listen.c \
    libvncclient/rfbproto.c \
    libvncclient/sockets.c \
    libvncclient/tls_none.c \
    zlib/adler32.c \
    zlib/compress.c \
    zlib/crc32.c \
    zlib/deflate.c \
    zlib/gzclose.c \
    zlib/gzlib.c \
    zlib/gzread.c \
    zlib/gzwrite.c \
    zlib/infback.c \
    zlib/inffast.c \
    zlib/inflate.c \
    zlib/inftrees.c \
    zlib/trees.c \
    zlib/uncompr.c \
    zlib/zutil.c \
    libjpeg/jaricom.c \
    libjpeg/jcapimin.c \
    libjpeg/jcapistd.c \
    libjpeg/jcarith.c \
    libjpeg/jccoefct.c \
    libjpeg/jccolor.c \
    libjpeg/jcdctmgr.c \
    libjpeg/jchuff.c \
    libjpeg/jcinit.c \
    libjpeg/jcmainct.c \
    libjpeg/jcmarker.c \
    libjpeg/jcmaster.c \
    libjpeg/jcomapi.c \
    libjpeg/jcparam.c \
    libjpeg/jcprepct.c \
    libjpeg/jcsample.c \
    libjpeg/jctrans.c \
    libjpeg/jdapimin.c \
    libjpeg/jdapistd.c \
    libjpeg/jdarith.c \
    libjpeg/jdatadst.c \
    libjpeg/jdatasrc.c \
    libjpeg/jdcoefct.c \
    libjpeg/jdcolor.c \
    libjpeg/jddctmgr.c \
    libjpeg/jdhuff.c \
    libjpeg/jdinput.c \
    libjpeg/jdmainct.c \
    libjpeg/jdmarker.c \
    libjpeg/jdmaster.c \
    libjpeg/jdmerge.c \
    libjpeg/jdpostct.c \
    libjpeg/jdsample.c \
    libjpeg/jdtrans.c \
    libjpeg/jerror.c \
    libjpeg/jfdctflt.c \
    libjpeg/jfdctfst.c \
    libjpeg/jfdctint.c \
    libjpeg/jidctflt.c \
    libjpeg/jidctfst.c \
    libjpeg/jidctint.c \
    libjpeg/jmemansi.c \
    libjpeg/jmemmgr.c \
    libjpeg/jquant1.c \
    libjpeg/jquant2.c \
    libjpeg/jutils.c \
    libjpeg/rdbmp.c \
    libjpeg/rdcolmap.c \
    libjpeg/rdppm.c \
    libjpeg/rdrle.c \
    libjpeg/rdswitch.c \
    libjpeg/rdtarga.c \
    libjpeg/transupp.c \
    libjpeg/wrbmp.c \
    libjpeg/wrgif.c \
    libjpeg/wrppm.c \
    libjpeg/wrrle.c \
    libjpeg/wrtarga.c \
    libvncclient/vncviewer.c \
    surfacewidget.cpp \
    qt2keysym.cpp \
    aboutdialog.cpp \
    rangeslider.cpp \
    surfacewidget_gl.cpp


HEADERS  += wrenchmainwindow.h \
            adbclient.h \
            adbstatethread.hpp \
            adbvncthread.hpp \
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
    phonescreensyncer.h \
    mainwindow.h \
    connectionwindow.h \
    qvncviewersettings.h \
    macros.h \
    surfacewidget.h \
    qt2keysum.h \
    aboutdialog.h \
    rangeslider.h \
    surfacewidget_gl.h \
    rfb/default8x16.h \
    rfb/keysym.h \
    rfb/rfb.h \
    rfb/rfbclient.h \
    rfb/rfbconfig.h \
    rfb/rfbint.h \
    rfb/rfbproto.h \
    rfb/rfbregion.h

FORMS    += wrenchmainwindow.ui \
            dialoggetentry.ui \
            phonescreendialog.ui \
            mainwindow.ui \
            connectionwindow.ui \
            aboutdialog.ui \
            rangeslider.ui

RESOURCES += Wrench.qrc \
             qvncviewer.qrc


