#include "adbnotificationthread.hpp"
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
#include <QtCore/QJsonDocument>
#include <QJsonObject>
#include <QtCore/QDateTime>


AdbNotificationThread::AdbNotificationThread(QObject* parent)
    : QThread(parent)
{
    notificationSocket = NULL;
    mConnectTimer = NULL;
}

AdbNotificationThread::~AdbNotificationThread()
{
    if (notificationSocket) {
        notificationSocket->close();
        delete notificationSocket;
    }
}

void AdbNotificationThread::timeOut()
{

}

void AdbNotificationThread::onDisconnected()
{
    if (!mConnectTimer) {
        mConnectTimer = new QTimer();
        mConnectTimer->setSingleShot(true);
        connect(mConnectTimer, SIGNAL(timeout()), this, SLOT(onDisconnected()));

    }
    emit adbNotificationState("Notification-Offline");

    // at the start, suppose the adb not connected.
    if (notificationSocket) {
        delete notificationSocket;
    }
    notificationSocket = new QTcpSocket();

    static QString adb_serial = QProcessEnvironment::systemEnvironment().value("ANDROID_SERIAL");

    if (adb_serial.isEmpty()) {
        AdbClient::doAdbForward("host:forward:tcp:58888;localabstract:WrenchNotifications");
    } else {
        AdbClient::doAdbForward("host-serial:" + adb_serial + ":forward:tcp:58888;localabstract:WrenchNotifications");
    }



    notificationSocket->connectToHost("127.0.0.1", 58888 + QProcessEnvironment::systemEnvironment().value("WRENCH_INSTANCE", "0").toInt(), QIODevice::ReadWrite);
    if (!notificationSocket->waitForConnected()) {
        mConnectTimer->start(1000);
        return;
    }

    if (!notificationSocket->waitForReadyRead(500)) {
        qDebug() << "Can't wait for notification init read\n";
        mConnectTimer->start(1000);
        return;
    }

    if (QString(notificationSocket->readLine()).contains("notification ready")) {
        notificationSocket->write("hello\n");
        notificationSocket->flush();
        connect(notificationSocket, SIGNAL(readChannelFinished()), this, SLOT(onDisconnected()), Qt::QueuedConnection);
        connect(notificationSocket, SIGNAL(readyRead()), this, SLOT(onNewNotification()));
        notificationSocket->write("list\n");
        emit adbNotificationState("Notification-Online");
    } else {
        qDebug() << "Can't read from notification";
        mConnectTimer->start(1000);
        return;
    }


}

void AdbNotificationThread::onNewNotification()
{
    QByteArray bytes = notificationSocket->readLine();
    qint64 currentTime = QDateTime::currentMSecsSinceEpoch();
    if (bytes == mLastJsonStr) {
        if (currentTime - mLastTime < 2000) {
            return;
        }
    }

    mLastJsonStr = bytes;
    mLastTime = currentTime;

    QJsonDocument jdoc = QJsonDocument::fromJson(bytes);
    if (!jdoc.isObject()) {
        qDebug() << "Not a json object" << jdoc;
        return;
    }

    QJsonObject jobj = jdoc.object();
    QString key = jobj.value("key").toString();
    QString pkg = jobj.value("pkg").toString();
    QString title = jobj.value("title").toString();
    QString text = jobj.value("text").toString();
    QString ticker = jobj.value("ticker").toString();

    emit adbNotificationArrived(key, pkg, title, text, ticker);
    qDebug() << key << "(" << pkg << "): " << title << "(" << text << ")";
    if (notificationSocket->bytesAvailable() > 0) {
        qDebug() << "Still more notifications readable";
        onNewNotification();
    }
}

void AdbNotificationThread::run()
{
    this->moveToThread(this);
    onDisconnected();
    exec();
}

void AdbNotificationThread::onAdbNotificationClicked(const QString& key)
{
    QString toWrite = "click " + key + "\n";
    notificationSocket->write(toWrite.toUtf8());
    notificationSocket->flush();
}
