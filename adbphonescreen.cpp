#include "adbphonescreen.hpp"
#include <QtCore/QProcess>
#include <QtCore/QProcessEnvironment>
#include <QtCore/QFile>
#include <QtCore/QDir>
#include <cassert>
#include <QtDebug>
#include <QtCore/QThread>
#include "bhj_help.hpp"

AdbPhoneScreenThread::AdbPhoneScreenThread(QObject* parent)
    : QThread(parent)
{
    shouldStop = 0;
    paused = 0;
}

AdbPhoneScreenThread::~AdbPhoneScreenThread()
{
}

void AdbPhoneScreenThread::stopIt()
{
    shouldStop = 1;
}

void AdbPhoneScreenThread::run()
{
    while (!shouldStop) {
        if (!paused) {
            system("set -x; the-true-adb shell screencap /sdcard/t1wrench-screen.png && the-true-adb pull /sdcard/t1wrench-screen.png;");
            emit phoneScreenUpdate();

        }
        for (int i = 0; i < 10; i++) {
            QThread::msleep(100);
            if (mAppState == Qt::ApplicationActive) {
                break;
            }
            if (i == 9) {
                qDebug() << "slept for 1 second";
            }
        }

    }
}

void AdbPhoneScreenThread::continueLoop()
{
    paused = 0;
}

void AdbPhoneScreenThread::pauseLoop()
{
    paused = 1;
}

void AdbPhoneScreenThread::setAppState(Qt::ApplicationState appState)
{
    mAppState = appState;
}
