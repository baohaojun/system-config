#include "t1wrenchmainwindow.h"

#ifdef Q_OS_WIN32
#include <windows.h>
#endif

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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <QtCore/QProcessEnvironment>

using namespace std;

#ifdef Q_OS_WIN32
void setenv(const char* name, const char* val, int overide)
{
    _putenv_s(name, val);
}
#endif

int main(int argc, char *argv[])
{

#ifdef Q_OS_WIN32
    HWND hwnd = GetConsoleWindow();
    if (hwnd) {
        ShowWindow(hwnd, SW_HIDE);
    }
#endif
    QString appDir = QCoreApplication::applicationDirPath();
    if (appDir.isEmpty()) { // happens on mac, =open T1Wrench.app=
        // google for "When OS X is set to use Japanese, a bug causes this sequence"
        char exe[PATH_MAX] = "";
        realpath(argv[0], exe);
        appDir = exe;
        appDir = appDir.left(appDir.lastIndexOf('/'));
    }
    chdir(qPrintable(appDir));

    QString pathEnv = appDir;
#ifdef Q_OS_WIN32
    pathEnv += ";";
#else
    pathEnv += ":";
#endif
    pathEnv += QProcessEnvironment::systemEnvironment().value("PATH");

    setenv("PATH", qPrintable(pathEnv), 1);
    // FILE* fp = fopen("/Users/bhj/t1.log", "w");
    // if (fp) {
    //     char cwd[1024];
    //     fprintf(fp, "cwd is %s, app is %s\n", getcwd(cwd, sizeof(cwd)), qPrintable(QCoreApplication::applicationDirPath()));
    //     if (QCoreApplication::applicationDirPath().isEmpty()) {
    //         fprintf(fp, "app path is ''\n");
    //     }
    //     fclose(fp);
    // }
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
    qApp->setStyleSheet(ba);
    cssFile.close();
    return a.exec();
}
