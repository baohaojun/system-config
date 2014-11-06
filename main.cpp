#include "t1wrenchmainwindow.h"
#include <QApplication>
#include <QPushButton>
#include <QFrame>
#include <string>
#include <QLineEdit>
#include <QPlainTextEdit>
#include "adbstatethread.hpp"
#include <QtCore/QFile>
#include <QtCore/QDir>
#include <QtCore/QByteArray>
using namespace std;

#ifdef Q_OS_WIN32
#include <windows.h>
#endif
int main(int argc, char *argv[])
{

#ifdef Q_OS_WIN32
    HWND hwnd = GetConsoleWindow();
    if (hwnd) {
        ShowWindow(hwnd, SW_HIDE);
    }
#endif

    QApplication a(argc, argv);
    T1WrenchMainWindow w;
    w.show();

    AdbStateThread adbState(&w);
    w.connect(&adbState, SIGNAL(adbStateUpdate(QString)), &w, SLOT(adbStateUpdated(QString)));
    adbState.start();

    QDir::addSearchPath("skin", ":/skin/default/resources");
    QDir::addSearchPath("config", ":config");
    QDir::addSearchPath("chatstyle", ":chatstyle/clear");

    QFile cssFile("skin:style.css");
    cssFile.open(QIODevice::ReadOnly | QIODevice::Text);
    QByteArray ba = cssFile.readAll();
    fprintf(stderr, "hello ba is %s\n", ba.constData());
    qApp->setStyleSheet(ba);
    cssFile.close();
    return a.exec();
}
