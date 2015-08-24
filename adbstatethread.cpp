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

QString getStcmdCmdResult(const QString& cmd, bool stripRetCode)
{
    QString res = getExecutionOutput("adb shell stcmd-subcase.sh " + cmd);
    if (stripRetCode && res.endsWith("ret: 0")) {
        res = res.left(res.length() - strlen("ret: 0"));
    }
    while (res.endsWith("\n")) {
        res.chop(1);
    }
    return res;
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
            qDebug() << "adb output is " << uname;
            if (lastAdbState != "Online") {
                lastAdbState = "Online";
                if (getExecutionOutput("the-true-adb shell screencap -h").contains("jpg")) {
                    gScreenCapJpg = 1;
                } else {
                    gScreenCapJpg = 0;
                }
            }
            emit adbStateUpdate("Online");
            if (getExecutionOutput("the-true-adb", args2).contains("Linux")) {
                //do nothing
            }
        } else {
            lastAdbState = "Offline";
            qDebug() << "Offline: adb output is " << uname;
            emit adbStateUpdate("Offline");
            QThread::msleep(500);
        }
    }
}
