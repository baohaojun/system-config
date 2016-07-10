#include "wrench.h"
#include "adbphonescreenthread.hpp"
#include <QtCore/QProcess>
#include <QtCore/QProcessEnvironment>
#include <QtCore/QFile>
#include <QtCore/QDir>
#include <cassert>
#include <QtDebug>
#include <QtCore/QThread>
#include "bhj_help.hpp"
#include <QTimer>

AdbPhoneScreenThread::AdbPhoneScreenThread(QObject* parent)
    : QThread(parent)
{
    mPhoneScreenDialog = (PhoneScreenDialog *)parent;
    shouldStop = 0;
    paused = 0;
    mScreenSyncer = 0;
}

AdbPhoneScreenThread::~AdbPhoneScreenThread()
{
    if (mScreenSyncer) {
        mScreenSyncer->deleteLater();
    }
}

void AdbPhoneScreenThread::stopIt()
{
    shouldStop = 1;
}

void AdbPhoneScreenThread::run()
{
    mScreenSyncer = new PhoneScreenSyncer();
    connect(mScreenSyncer, SIGNAL(phoneScreenUpdate()), mPhoneScreenDialog, SLOT(phoneScreenUpdated()));
    connect(this, SIGNAL(dialogShow(bool)), mScreenSyncer, SLOT(startSyncing(bool)));
    connect(this, SIGNAL(requestSyncScreen()), mScreenSyncer, SLOT(syncScreen()));
    mScreenSyncer->syncScreen();
    exec();
}

void AdbPhoneScreenThread::continueLoop()
{
    emit dialogShow(true);
}

void AdbPhoneScreenThread::pauseLoop()
{
    emit dialogShow(false);
}

void AdbPhoneScreenThread::syncScreen()
{
    emit requestSyncScreen();
}
