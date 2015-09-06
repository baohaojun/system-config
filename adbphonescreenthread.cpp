#include "t1wrench.h"
#include "adbphonescreenthread.hpp"
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
            const char *screenFile = "t1wrench-screen.png";
            if (gScreenCapJpg) {
                screenFile = "t1wrench-screen.jpg";
            }

#ifdef Q_OS_WIN32
            qSystem(QString().sprintf("the-true-adb shell screencap /sdcard/%s", screenFile));
            qSystem(QString().sprintf("the-true-adb pull /sdcard/%s", screenFile));
#else
            qSystem(QString().sprintf("the-true-adb shell screencap /sdcard/%s && the-true-adb pull /sdcard/%s;", screenFile, screenFile));
#endif
            emit phoneScreenUpdate();

        }
        for (int i = 0; i < 10; i++) {
            QThread::msleep(100);
            if (mAppState == Qt::ApplicationActive) {
                break;
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
