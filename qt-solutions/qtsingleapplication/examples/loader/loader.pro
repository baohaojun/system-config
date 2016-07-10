TEMPLATE	= app

contains(QT_VERSION, ^5.*) {
    QT *= printsupport
}

include(../../src/qtsingleapplication.pri)

SOURCES		+= main.cpp
