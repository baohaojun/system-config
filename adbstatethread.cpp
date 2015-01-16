#include "adbstatethread.hpp"
#include <QtCore/QProcess>
#include <QtCore/QProcessEnvironment>
#include <QtCore/QFile>
#include <QtCore/QDir>
#include <cassert>
#include <QtDebug>
#include <QtCore/QThread>
#include "bhj_help.hpp"

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

    while (1) {
        // at the start, suppose the adb not connected.
        QString uname = getExecutionOutput("the-true-adb shell sh -c 'if test \"$(getprop sys.boot_completed)\" = 1; then uname || busybox uname || { echo -n Lin && echo -n ux; }; fi'");
        if (uname.contains("Linux")) {
            qDebug() << "adb output is " << uname;
            emit adbStateUpdate("Online");
            if (getExecutionOutput("the-true-adb", QStringList() << "shell" << "sh" << "-c" << "sleep 300; uname || busybox uname || { echo -n Lin && echo -n ux; }").contains("Linux")) {
                //do nothing
            }
        } else {
            qDebug() << "Offline: adb output is " << uname;
            emit adbStateUpdate("Offline");
            QThread::msleep(500);
        }
    }
}
