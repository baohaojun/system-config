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
        if (mAdbInput)
            delete mAdbInput;
        AdbClient::doAdbShell("am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei getapk 1");
        mAdbInput = AdbClient::doAdbPipe("sh /sdcard/setclip-apk.txt app_process /system/bin/ Input");
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
