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
        system("the-true-adb shell screencap /sdcard/t1wrench-screen.png && the-true-adb pull /sdcard/t1wrench-screen.png;");
        emit phoneScreenUpdate();
        QThread::msleep(100);
    }
}
