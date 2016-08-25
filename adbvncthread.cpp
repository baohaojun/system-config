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
#include <QTime>

AdbVncThread::AdbVncThread(QObject* parent)
    : QThread(parent)
{
    mAdbInput = NULL;
    mAdbInputFinished = false;
    mConnectTimer = NULL;
}

AdbVncThread::~AdbVncThread()
{
}


void AdbVncThread::onInputDataReady()
{
    qDebug() << mAdbInput->getSock()->readAll();
    emit adbVncUpdate("Online");
}

void AdbVncThread::inputServerFinished()
{
    mAdbInputFinished = true;
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

    if (!mAdbInput || mAdbInput && mAdbInputFinished) {
        if (mAdbInput) {
            delete mAdbInput;
            mAdbInput = NULL;
        }

        mAdbInput = AdbClient::doAdbPipe("/data/data/com.android.shell/androidvncserver");
        mAdbInputFinished = false;
        connect(mAdbInput->getSock(), SIGNAL(readyRead()), this, SLOT(onInputDataReady()));
        connect(mAdbInput->getSock(), SIGNAL(readChannelFinished()), this, SLOT(inputServerFinished()));
        connect(mAdbInput->getSock(), SIGNAL(readChannelFinished()), this, SLOT(onDisconnected()));
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
