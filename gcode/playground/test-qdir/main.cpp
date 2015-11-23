#include <QCoreApplication>
#include <QProcessEnvironment>
#include <QDebug>
#include <QDir>

#include <QDebug>
#include <QSettings>
#include <QProcessEnvironment>
#include <QDir>
#include <QFile>
#include <QList>
#include <QStandardPaths>

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    QString str = QStandardPaths::locate(QStandardPaths::ConfigLocation, "T1Wrench", QStandardPaths::LocateDirectory);
    QDir::
    printf("hello %s\n", qPrintable(str));
    return 0;
}
