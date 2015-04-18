/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2014  Patrick von Reth <vonreth@kde.org>

    SnoreNotify is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SnoreNotify is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with SnoreNotify.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "snarlnetwork.h"
#include "libsnore/snore.h"

#include <QTcpServer>
#include <QTcpSocket>

#include <iostream>
using namespace Snore;

SnarlNetworkFrontend::SnarlNetworkFrontend():
    SnoreFrontend("SnarlNetwork")
{

}

SnarlNetworkFrontend::~SnarlNetworkFrontend()
{
}

bool SnarlNetworkFrontend::initialize()
{
    parser = new Parser(this);
    tcpServer = new QTcpServer(this);
    if (!tcpServer->listen(QHostAddress::Any, port)) {
        snoreDebug(SNORE_DEBUG) << "The port is already used";
        return false;
    } else {
        connect(tcpServer, SIGNAL(newConnection()), this, SLOT(handleConnection()));
        std::cout << "The Snarl Network Protokoll is developed for Snarl <http://www.fullphat.net/>" << std::endl;
    }
    return SnoreFrontend::initialize();
}

bool SnarlNetworkFrontend::deinitialize()
{
    if (SnoreFrontend::deinitialize()) {
        parser->deleteLater();
        parser = NULL;

        tcpServer->deleteLater();
        tcpServer = NULL;
        return true;
    }
    return false;
}

void SnarlNetworkFrontend::slotActionInvoked(Snore::Notification notification)
{
    snoreDebug(SNORE_DEBUG) << notification.closeReason();
    callback(notification, "SNP/1.1/304/Notification acknowledged/");
}

void SnarlNetworkFrontend::slotNotificationClosed(Snore::Notification notification)
{
    switch (notification.closeReason()) {
    case Notification::TIMED_OUT:
        callback(notification, "SNP/1.1/303/Notification timed out/");
        break;
    case Notification::ACTIVATED:
        callback(notification, "SNP/1.1/307/Notification closed/");
        break;
    case Notification::DISMISSED:
        callback(notification, "SNP/1.1/302/Notification cancelled/");
        break;
    }
}

void SnarlNetworkFrontend::handleConnection()
{
    QTcpSocket *client = tcpServer->nextPendingConnection();
    connect(client, SIGNAL(readyRead()), this, SLOT(handleMessages()));
    connect(client, SIGNAL(disconnected()), client, SLOT(deleteLater()));
}

void SnarlNetworkFrontend::handleMessages()
{
    const QString out("SNP/1.1/0/OK");
    QTcpSocket *client = qobject_cast<QTcpSocket *>(sender());

    QStringList messages(QString::fromLatin1(client->readAll()).trimmed().split("\r\n"));
    foreach(const QString & s, messages) {
        if (s.isEmpty()) {
            continue;
        }
        Notification noti;
        parser->parse(noti, s, client);
        if (noti.isValid()) {
            SnoreCore::instance().broadcastNotification(noti);
            write(client, QString("%1/%2\r\n").arg(out, QString::number(noti.id())));
        } else {
            write(client, QString("%1\r\n").arg(out));
        }
    }
}

void SnarlNetworkFrontend::callback(Notification &sn, const QString msg)
{
    if (sn.hints().containsPrivateValue(this, "clientSocket")) {
        QTcpSocket *client = qobject_cast<QTcpSocket *>(sn.hints().privateValue(this, "clientSocket").value<QObject *>());
        write(client, QString("%1%2\r\n").arg(msg, QString::number(sn.id())));
    }
}

