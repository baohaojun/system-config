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
}

AdbPhoneScreenThread::~AdbPhoneScreenThread()
{
}

void AdbPhoneScreenThread::run()
{
    while (true) {
        system("the-true-adb shell screencap /sdcard/t1wrench-screen.png && the-true-adb pull /sdcard/t1wrench-screen.png;");
        emit phoneScreenUpdate();
        QThread::msleep(100);
    }
}
