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
#include "core/snore.h"

#include <QtCore>
#include <QTcpServer>
#include <QTcpSocket>

#include <iostream>
using namespace Snore;

Q_EXPORT_PLUGIN2(libsnore_frontend_snarlnetwork,SnarlNetworkFrontend)


SnarlNetworkFrontend::SnarlNetworkFrontend():
    SnoreFrontend("SnarlNetwork")
{

}

SnarlNetworkFrontend::~SnarlNetworkFrontend(){
}

bool SnarlNetworkFrontend::initialize(SnoreCore *snore){
    parser = new Parser(this);
    tcpServer = new QTcpServer(this);
    if(!tcpServer->listen(QHostAddress::Any,port))
    {
        snoreDebug( SNORE_DEBUG )<<"The port is already used";
        return false;
    }
    else
    {
        connect(tcpServer, SIGNAL(newConnection()), this, SLOT(handleConnection()));
        std::cout<<"The Snarl Network Protokoll is developed for Snarl <http://www.fullphat.net/>"<<std::endl;
    }
    return SnoreFrontend::initialize(snore);
}

bool SnarlNetworkFrontend::deinitialize()
{
    if(SnoreFrontend::deinitialize())
    {
        parser->deleteLater();
        parser = NULL;

        tcpServer->deleteLater();
        tcpServer = NULL;
        return true;
    }
    return false;
}


void SnarlNetworkFrontend::actionInvoked(Snore::Notification notification)
{
    //TODO:fix callback
    if(notification.actionInvoked().id() == 1 )
    {
        callback(notification,"SNP/1.1/304/Notification acknowledged/");
    }
    else if(notification.actionInvoked().id() == 2)
    {
        callback(notification,"SNP/1.1/302/Notification cancelled/");
    }
}
void SnarlNetworkFrontend::notificationClosed(Snore::Notification notification)
{
    if(notification.closeReason() == Notification::TIMED_OUT)
    {
        callback(notification, "SNP/1.1/303/Notification timed out/");
    }
    else
    {
        callback(notification, "SNP/1.1/307/Notification closed/");
    }
}

void SnarlNetworkFrontend::handleConnection()
{
    QTcpSocket *client = tcpServer->nextPendingConnection();
    connect(client,SIGNAL(readyRead()),this,SLOT(handleMessages()));
    connect(client,SIGNAL(disconnected()), client, SLOT(deleteLater()));
}

void SnarlNetworkFrontend::handleMessages()
{
    const QString out("SNP/1.1/0/OK");
    QTcpSocket *client = qobject_cast<QTcpSocket*>(sender());

    QStringList messages(QString::fromAscii(client->readAll()).trimmed().split("\r\n"));
    foreach(const QString &s, messages)
    {
        if(s.isEmpty())
        {
            continue;
        }
        Notification noti;
        parser->parse(noti, s, client);
        if(noti.isValid())
        {
            snore()->broadcastNotification(noti);
            write(client, QString("%1/%2\r\n").arg(out,QString::number(noti.id())));
        }
        else
        {
            write(client, QString("%1\r\n").arg(out));
        }
    }
}

void SnarlNetworkFrontend::callback(Notification &sn, const QString msg)
{
    QTcpSocket *client = myQVariantCast<QTcpSocket*>(sn.hints().privateValue(this, "clientSocket"));
    if(client && !msg.isEmpty())
    {
        write(client, QString("%1%2\r\n").arg(msg, QString::number(sn.id())));
    }
}

