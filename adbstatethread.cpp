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
    pingReplyOK = false;
    pingSocket = NULL;
    mAdbInput = NULL;
    mAdbInputFinished = false;
    mConnectTimer = NULL;
}

AdbStateThread::~AdbStateThread()
{
}


void AdbStateThread::onInputDataReady()
{
    qDebug() << mAdbInput->getSock()->readAll();
}

void AdbStateThread::canPingInputServer()
{

}

void AdbStateThread::timeOut()
{

}

void AdbStateThread::inputServerFinished()
{
    mAdbInputFinished = true;
}

void AdbStateThread::onDisconnected()
{
    qDebug() << "onDisconnected" << QThread::currentThreadId();
    if (!mConnectTimer) {
        mConnectTimer = new QTimer();
        mConnectTimer->setSingleShot(true);
        connect(mConnectTimer, SIGNAL(timeout()), this, SLOT(onDisconnected()));

    }
    // at the start, suppose the adb not connected.
    QStringList args1 = QStringList() << "sh" << "-c" << "if test \"$(getprop sys.boot_completed)\" = 1; then { echo -n Lin && echo -n ux; }; fi";


    QString uname = adb_quote_shell(args1);
    static QString lastAdbState;
    emit adbStateUpdate("Offline");
    if (!uname.contains("Linux")) {
        lastAdbState = "Offline";
        mConnectTimer->start(1000);
        return;
    }

    if (lastAdbState != "Online") {
        lastAdbState = "Online";
        QString screencapHelp = adb_quote_shell(QStringList() << "screencap" << "-h");
        if (screencapHelp.contains("jpg")) {
            gScreenCapJpg = 1;
        } else {
            gScreenCapJpg = 0;
        }
    }

    if (pingSocket) {
        delete pingSocket;
        pingReplyOK = false;
    }
    pingSocket = new QTcpSocket();

    if (!mAdbInput || mAdbInput && mAdbInputFinished) {
        if (mAdbInput) {
            delete mAdbInput;
            mAdbInput = NULL;
        }

        QString res = AdbClient::doAdbShell("rm /sdcard/setclip-apk.txt;"
                                            "am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei getapk 1;"
                                            "for x in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do "
                                            "if test -e /sdcard/setclip-apk.txt; then echo -n ye && echo s; break; fi; sleep .1; done");
        if (!res.contains("yes")) {
            emit adbStateInfo("prompt", "需要重新安装小扳手辅助apk，请允许");
            if (!AdbClient::doAdbPush("Setclip.apk", "/data/local/tmp/Setclip.apk")) {
                emit adbStateInfo("info", "无法上传小扳手辅助apk");
                mConnectTimer->start(1000);
                return;
            }
            QString res = AdbClient::doAdbShell("pm install -r /data/local/tmp/Setclip.apk");
            if (!res.contains("\nSuccess")) {
                if (res.contains("INSTALL_FAILED_UNKNOWN_SOURCES")) {
                    emit adbStateInfo("prompt",
                                      "安装小扳手辅助apk失败，请检查手机安全设置是否禁止通过USB安装:\n\n"
                                      "（在Smartisan手机上，打开：设置->安全中心->高级设置->应用程序安装来源管理，勾选“未知来源”）\n\n"
                                      + res);
                } else {
                    emit adbStateInfo("prompt", "安装小扳手辅助apk失败，请检查手机安全设置是否禁止通过USB安装:\n\n" + res);
                }
                mConnectTimer->start(1000);
                return;
            } else {
                emit adbStateInfo("info", "小扳手辅助apk安装成功");
            }
        }
        mAdbInput = AdbClient::doAdbPipe("if test -e /sdcard/setclip-apk.txt; then sh /sdcard/setclip-apk.txt app_process /system/bin/ Input; else echo apk not found?; fi");
        mAdbInputFinished = false;
        connect(mAdbInput->getSock(), SIGNAL(readyRead()), this, SLOT(onInputDataReady()));
        connect(mAdbInput->getSock(), SIGNAL(readChannelFinished()), this, SLOT(inputServerFinished()));
    }

    AdbClient::doAdbForward("host:forward:tcp:28888;localabstract:T1Wrench");
    pingSocket->connectToHost("localhost", 28888, QIODevice::ReadWrite);
    if (!pingSocket->waitForConnected()) {
        qDebug() << "can't connect ping";
        mConnectTimer->start(1000);
        return;
    }
    if (!_writex(*pingSocket, "ping\n", 5)) {
        qDebug() << "Can't write ping";
        mConnectTimer->start(1000);
        return;
    }

    if (!pingSocket->waitForReadyRead(500)) {
        qDebug() << "Can't wait for read\n";
        mConnectTimer->start(1000);
        return;
    }

    if (QString(pingSocket->readLine(100)).contains("input ok")) {
        emit adbStateUpdate("Online");
        connect(pingSocket, SIGNAL(readChannelFinished()), this, SLOT(onDisconnected()));
    } else {
        qDebug() << "Can't read";
        mConnectTimer->start(1000);
        return;
    }


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

QString shell_quote(const QString& str)
{
    QString res = str;
    res.replace("'", "'\\''");
    res = "'" + res + "'";
    return res;
}

QString adb_quote_shell(const QStringList& args)
{
    QStringList qargs;
    foreach (const QString& arg, args) {
        qargs << shell_quote(arg);
    }
    return AdbClient::doAdbShell(qargs);
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
    this->moveToThread(this);
    onDisconnected();
    exec();
}
