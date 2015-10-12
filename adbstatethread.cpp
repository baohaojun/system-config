#include "adbstatethread.hpp"
#include <QtCore/QProcess>
#include <QtCore/QProcessEnvironment>
#include <QtCore/QFile>
#include <QtCore/QDir>
#include <cassert>
#include <QtDebug>
#include <QtCore/QThread>
#include "bhj_help.hpp"
#include "t1wrench.h"
#include <QTime>

bool gScreenCapJpg;

AdbStateThread::AdbStateThread(QObject* parent)
    : QThread(parent)
{
}

AdbStateThread::~AdbStateThread()
{
}

void die(const char* how)
{
    fprintf(stderr, "%s\n", how);
    exit(1);
}

QString getExecutionOutput(const QString& cmd, const QStringList& args)
{
    QProcess p;
    p.setStandardOutputFile(QDir::temp().filePath(QString().sprintf("adb-state-repair-center.%lx", long(QThread::currentThreadId()))));
    p.setStandardErrorFile(QDir::temp().filePath(QString().sprintf("adb-state-repair-center.%lx", long(QThread::currentThreadId()))));
    p.start(cmd, args);
    if (!p.waitForFinished(-1)) {
        p.kill();
        p.waitForFinished();
    } else {
        QFile result(QDir::temp().filePath(QString().sprintf("adb-state-repair-center.%lx", long(QThread::currentThreadId()))));
        result.open(QIODevice::ReadOnly);
        QString ret = result.readAll();
        ret.replace("\r", "");
        while (ret.endsWith("\n")) {
            ret.chop(1);
        }
        result.close();
        result.remove();
        return ret;
    }
    return "";

}

QString getExecutionOutput(const QString& cmd)
{
    if (cmd.contains(" ")) {
        QStringList args = cmd.split(" ");
        QString new_cmd = args.front();
        args.pop_front();
        return getExecutionOutput(new_cmd, args);
    } else
        return getExecutionOutput(cmd, QStringList());
}

void AdbStateThread::run()
{

    QDir tmpdir = QDir::temp();
    QString version = getExecutionOutput("the-true-adb version");
    bool adb_1031 = 0;
    if (version.contains("1.0.31")) {
        adb_1031 = 1;
    }

    while (1) {
        // at the start, suppose the adb not connected.
        QStringList args1 = QStringList() << "shell" << "sh" << "-c" << "'if test \"$(getprop sys.boot_completed)\" = 1; then uname || busybox uname || { echo -n Lin && echo -n ux; }; fi'";
        QStringList args2 = QStringList() << "shell" << "sh" << "-c" << "'sleep 300; uname || busybox uname || { echo -n Lin && echo -n ux; }'";
        if (adb_1031) {
            QString last = args1.last();
            args1.pop_back();
            args1 += last.replace('\'', ' ');

            last = args2.last();
            args2.pop_back();
            args2 += last.replace('\'', ' ');
        }

        QString uname = getExecutionOutput("the-true-adb", args1);
        static QString lastAdbState;
        if (uname.contains("Linux")) {
            if (lastAdbState != "Online") {
                lastAdbState = "Online";
                QString screencapHelp = getExecutionOutput("the-true-adb shell screencap -h");
                qDebug() << screencapHelp;
                if (screencapHelp.contains("jpg")) {
                    gScreenCapJpg = 1;
                } else {
                    gScreenCapJpg = 0;
                }
            }
            emit adbStateUpdate("Online");
            system("the-true-adb shell am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei getapk 1");
            system("the-true-adb forward tcp:28888 localabstract:T1Wrench");
            QString env = getExecutionOutput("the-true-adb shell cat /sdcard/setclip-apk.txt");
            QTime t;
            t.start();
            qSystem("the-true-adb shell sh /sdcard/setclip-apk.txt app_process /system/bin/ Input");
            if (t.elapsed() < 2000) {
                QThread::msleep(30000);
            }
        } else {
            lastAdbState = "Offline";
            emit adbStateUpdate("Offline");
            QThread::msleep(500);
        }
    }
}
