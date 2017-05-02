#include "adbvncthread.hpp"
#include <QtCore/QProcess>
#include <QtCore/QProcessEnvironment>
#include <QtCore/QFile>
#include <QtCore/QDir>
#include <cassert>
#include <QtDebug>
#include <QtCore/QThread>
#include "bhj_help.hpp"
#include "wrench.h"
#include "wrenchext.h"
#include <QTime>

AdbVncThread::AdbVncThread(QObject* parent)
    : QThread(parent)
{
    mAdbVncCmd = NULL;
    mAdbVncCmdFinished = false;
    mConnectTimer = NULL;
}

AdbVncThread::~AdbVncThread()
{
}


void AdbVncThread::onVncCmdDataReady()
{
    qDebug() << mAdbVncCmd->getSock()->readAll();
    emit adbVncUpdate("Online");
}

void AdbVncThread::vncCmdServerFinished()
{
    mAdbVncCmdFinished = true;
}

void AdbVncThread::onDisconnected()
{
    if (!mConnectTimer) {
        mConnectTimer = new QTimer(this);
        mConnectTimer->setSingleShot(true);
        connect(mConnectTimer, SIGNAL(timeout()), this, SLOT(onDisconnected()));

    }

    if (!gPhoneScreenSyncOn) {
        mConnectTimer->start(1000);
        return;
    }
    // at the start, suppose the adb not connected.
    QStringList args1 = QStringList() << "sh" << "-c" << "if test \"$(getprop sys.boot_completed)\" = 1; then { echo -n Lin && echo -n ux; /data/data/com.android.shell/busybox killall androidvncserver; }; fi";


    QString uname = adb_quote_shell(args1);
    static QString lastAdbVnc;
    emit adbVncUpdate("Offline");
    if (!uname.contains("Linux")) {
        lastAdbVnc = "Offline";
        mConnectTimer->start(1000);
        return;
    }

    if (lastAdbVnc != "Online") {
        lastAdbVnc = "Online";
    }

    if (!mAdbVncCmd || mAdbVncCmd && mAdbVncCmdFinished) {
        if (mAdbVncCmd) {
            delete mAdbVncCmd;
            mAdbVncCmd = NULL;
        }

        WrenchExt wrenchExt;

        QString vncCommand = wrenchExt.getConfig("vnc-server-command");
        if (vncCommand.isEmpty()) {
            vncCommand = "/data/data/com.android.shell/androidvncserver";
        }

        mAdbVncCmd = AdbClient::doAdbPipe(vncCommand);
        mAdbVncCmdFinished = false;
        connect(mAdbVncCmd->getSock(), SIGNAL(readyRead()), this, SLOT(onVncCmdDataReady()));
        connect(mAdbVncCmd->getSock(), SIGNAL(readChannelFinished()), this, SLOT(vncCmdServerFinished()));
        connect(mAdbVncCmd->getSock(), SIGNAL(readChannelFinished()), this, SLOT(onDisconnected()), Qt::QueuedConnection);
    }

    static QString adb_serial = QProcessEnvironment::systemEnvironment().value("ANDROID_SERIAL");

    if (adb_serial.isEmpty()) {
        AdbClient::doAdbForward("host:forward:tcp:5901;tcp:5901");
    } else {
        AdbClient::doAdbForward("host-serial:" + adb_serial + ":forward:tcp:5901;tcp:5901");
    }
}

void AdbVncThread::run()
{
    this->moveToThread(this);
    QTimer::singleShot(0, this, SLOT(onDisconnected()));
    exec();
}
