#include <QtGui/QApplication>
#include "mainwindow.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    DBusBinding::registerTypes();
    MainWindow w;
    w.show();
    return a.exec();
}
