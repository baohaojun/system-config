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

Q_EXPORT_PLUGIN2(snalnetwork,SnarlNetworkFrontend)


SnarlNetworkFrontend::SnarlNetworkFrontend():
    SnoreFrontend("SnarlNetwork")
{

}

SnarlNetworkFrontend::~SnarlNetworkFrontend(){
}

bool SnarlNetworkFrontend::init(SnoreCore *snore){
    parser=new Parser(this);
    tcpServer=new QTcpServer(this);
    if(!tcpServer->listen(QHostAddress::Any,port)){
        qDebug()<<"The port is already used";
        return false;
    }else{
        connect(tcpServer, SIGNAL(newConnection()), this, SLOT(handleConnection()));
        std::cout<<"The Snarl Network Protokoll is developed for Snarl <http://www.fullphat.net/>"<<std::endl;
    }
    return SnoreFrontend::init(snore);
}


void SnarlNetworkFrontend::actionInvoked(Notification notification)
{
    //TODO:fix callback
    SnarlNotification sn=notifications.value(notification.id());
    if(notification.actionInvoked().id() == 1 )
    {
        callback(sn,"SNP/1.1/304/Notification acknowledged/");
    }
    else if(notification.actionInvoked().id() == 2)
    {
        callback(sn,"SNP/1.1/302/Notification cancelled/");
    }
}
void SnarlNetworkFrontend::notificationClosed(Notification notification)
{
    SnarlNotification sn=notifications.value(notification.id());
    if(notification.closeReason() == NotificationEnums::CloseReasons::TIMED_OUT)
    {
        callback(sn,"SNP/1.1/303/Notification timed out/");
    }
    else
    {
        callback(sn,"SNP/1.1/307/Notification closed/");
    }
}

void SnarlNetworkFrontend::handleConnection(){
    QTcpSocket *client = tcpServer->nextPendingConnection();
    connect(client,SIGNAL(readyRead()),this,SLOT(handleMessages()));
    connect(client,SIGNAL(disconnected()), client, SLOT(deleteLater()));
}

void SnarlNetworkFrontend::handleMessages(){
    QString out("SNP/1.1/0/OK");
    QTcpSocket *client=qobject_cast<QTcpSocket*>(sender());
    QStringList incommings(QString::fromUtf8(client->readAll()).split("\r\n"));
    foreach(const QString &msg,incommings){
        QString s=msg.trimmed();
        if(s == "")
            continue;
        SnarlNotification noti=parser->parse(s,client);
        if(!noti.vailid)
            continue;
        if(noti.isNotification){
            snore()->broadcastNotification(noti.notification);
            if(noti.notification.id()!=0){
                out+="/"+QString::number(noti.notification.id());
                notifications.insert(noti.notification.id(),noti);
            }
        }
        out+="\r\n";

        client->write(out.toUtf8());
        if(noti.httpClient){
            client->disconnectFromHost();
            client->waitForDisconnected();
        }
        qDebug()<<out;
    }
}

void SnarlNetworkFrontend::callback(const SnarlNotification &sn,QString msg)
{
    notifications.remove(sn.notification.id());
    if(sn.clientSocket!=NULL&&!msg.isEmpty()){
        msg+=QString::number(sn.notification.id());
        qDebug()<<msg;
        sn.clientSocket->write(msg.toAscii()+"\r\n");
        sn.clientSocket->flush();

        if(sn.httpClient){
            sn.clientSocket->waitForBytesWritten(-1);
            sn.clientSocket->disconnectFromHost();
        }
    }
}

