#include "t1wrenchmainwindow.h"
#include <QApplication>
#include <QPushButton>
#include <QFrame>
#include <string>
#include <QLineEdit>
#include <QPlainTextEdit>
#include "adbstatethread.hpp"
using namespace std;

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    T1WrenchMainWindow w;
    w.show();

    AdbStateThread adbState(&w);
    w.connect(&adbState, SIGNAL(adbStateUpdate(QString)), &w, SLOT(adbStateUpdated(QString)));
    adbState.start();
    return a.exec();
}
