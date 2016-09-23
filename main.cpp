#include <QApplication>
#include <QPushButton>
#include <QFrame>
#include <string>
#include <QLineEdit>
#include <QTextEdit>
#include "adbstatethread.hpp"
#include <QtCore/QFile>
#include <QtCore/QDir>
#include <QtCore/QByteArray>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <QtCore/QProcessEnvironment>
#include <QtCore/QDebug>
#include <QStandardPaths>
#include <QtSingleApplication>
#include <QFileOpenEvent>

#ifdef Q_OS_WIN
#include <windows.h>
#endif

#include "fileopenfilter.h"
#include <QProcess>
#include "wrench.h"

#include "macros.h"
#include "wrenchmainwindow.h"
#include "vncmainwindow.h"
#include "qvncviewersettings.h"

using namespace std;

#ifdef Q_OS_WIN32
void setenv(const char* name, const char* val, int overide)
{
    QString str;
    str.sprintf("%s=%s", name, val);
    _putenv(qPrintable(str));
}
#endif

QtVncViewerSettings *globalConfig = 0;
VncMainWindow *vncMainWindow = 0;

int main(int argc, char *argv[])
{
    QtSingleApplication a(argc, argv);
    if (a.isRunning()) {
        if (argc == 2) {
            a.sendMessage(argv[1]);
        } else {
            a.sendMessage("");
        }
        return 0;
    }

#ifdef Q_OS_DARWIN
    FileOpenFilter fof(NULL);
    a.installEventFilter(&fof);
#endif

    globalConfig = new QtVncViewerSettings();
    globalConfig->setApplicationVersion(QVNCVIEWER_APP_VERSION);


#ifdef Q_OS_WIN32
    HWND hwnd = GetConsoleWindow();
    if (hwnd) {
        ShowWindow(hwnd, SW_HIDE);
    }
#endif
    QString appDir = QCoreApplication::applicationDirPath();
    if (appDir.isEmpty()) { // happens on mac, =open Wrench.app=
        // google for "When OS X is set to use Japanese, a bug causes this sequence"
#ifndef Q_OS_WIN32
        char exe[PATH_MAX] = "";
        realpath(argv[0], exe);
        appDir = exe;
        appDir = appDir.left(appDir.lastIndexOf('/'));
#endif
    }
    qDebug() << "appDir is " << appDir;
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

#ifndef Q_OS_WIN32
    QString home = QProcessEnvironment::systemEnvironment().value("HOME");
    if (home.isEmpty()) {
        home = QDir::homePath();
        qDebug() << "home is " << home << " (from qt)";
        setenv("HOME", qPrintable(home), 1);
    }
#endif
    WrenchMainWindow w;
    a.setActivationWindow(&w);
    w.show();

    QObject::connect(&a, SIGNAL(messageReceived(const QString&)),
                     &w, SLOT(startTask(const QString&)));
    QObject::connect(&w, SIGNAL(activateWindow()),
                     &a, SLOT(activateWindow()));
    if (argc == 2) {
        w.startTask(argv[1]);
    }

    AdbStateThread adbState;
    adbState.moveToThread(&adbState);
    w.connect(&adbState, SIGNAL(adbStateUpdate(QString)), &w, SLOT(adbStateUpdated(QString)));
    w.connect(&adbState, SIGNAL(adbStateInfo(QString, QString)), &w, SLOT(onInfoUpdate(QString, QString)), Qt::BlockingQueuedConnection);
    adbState.start();

    QDir::addSearchPath("skin", ":/skin/default/resources");
    QDir::addSearchPath("config", ":config");
    QDir::addSearchPath("chatstyle", ":chatstyle/clear");

    QProcess::startDetached("./the-true-adb", QStringList("start-server"));

    QString str = QStandardPaths::locate(QStandardPaths::GenericConfigLocation, ".", QStandardPaths::LocateDirectory);
    QDir configDir(str);
    configDir.mkdir("Wrench");
    str = QStandardPaths::locate(QStandardPaths::GenericConfigLocation, "Wrench", QStandardPaths::LocateDirectory);
    configDirPath = str;

    int result = a.exec();
    globalConfig->deleteLater();
    return result;
}

QString configDirPath;
