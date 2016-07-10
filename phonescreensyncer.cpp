#include "phonescreensyncer.h"
#include "wrench.h"

PhoneScreenSyncer::PhoneScreenSyncer(QObject *parent) : QObject(parent)
{
    qRegisterMetaType<Qt::ApplicationState>("Qt::ApplicationState");
    connect(qApp, SIGNAL(applicationStateChanged(Qt::ApplicationState)), this, SLOT(on_applicationStateChanged(Qt::ApplicationState)));
    connect(&mTimer, SIGNAL(timeout()), this, SLOT(syncScreen()));
    mTimer.setInterval(1000);
    mTimer.start();
}

void PhoneScreenSyncer::syncScreen()
{
    const char *screenFile = "wrench-screen.png";
    if (gScreenCapJpg) {
        screenFile = "wrench-screen.jpg";
    }

#ifdef Q_OS_WIN32
    qSystem(QString().sprintf("the-true-adb shell screencap /sdcard/%s", screenFile));
    qSystem(QString().sprintf("the-true-adb pull /sdcard/%s", screenFile));
#else
    qSystem(QString().sprintf("the-true-adb shell screencap /sdcard/%s >/dev/null && the-true-adb pull /sdcard/%s >/dev/null 2>&1", screenFile, screenFile));
#endif
    emit phoneScreenUpdate();
}

void PhoneScreenSyncer::on_applicationStateChanged(Qt::ApplicationState state)
{
    if (state == Qt::ApplicationActive) {
        mTimer.setInterval(1000);
    } else {
        mTimer.setInterval(5000);
    }
}

void PhoneScreenSyncer::startSyncing(bool start)
{
    if (start) {
        mTimer.start();
    } else {
        mTimer.stop();
    }
}
