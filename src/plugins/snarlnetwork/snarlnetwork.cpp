/****************************************************************************************
 * Copyright (c) 2010 Patrick von Reth <patrick.vonreth@gmail.com>                      *
 *                                                                                      *
 * This program is free software; you can redistribute it and/or modify it under        *
 * the terms of the GNU General Public License as published by the Free Software        *
 * Foundation; either version 2 of the License, or (at your option) any later           *
 * version.                                                                             *
 *                                                                                      *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY      *
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A      *
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.             *
 *                                                                                      *
 * You should have received a copy of the GNU General Public License along with         *
 * this program.  If not, see <http://www.gnu.org/licenses/>.                           *
 ****************************************************************************************/

#include "snarlnetwork.h"
#include "core/snoreserver.h"

#include <QtCore>
#include <QTcpServer>
#include <QTcpSocket>

#include <iostream>

Q_EXPORT_PLUGIN2(snalnetwork,SnarlNetworkFrontend)

SnarlNetworkFrontend::SnarlNetworkFrontend(SnoreServer *snore):
Notification_Frontend("SnarlNetworkFrontend",snore)
{
    parser=new Parser(this);
    tcpServer=new QTcpServer(this);
    if(!tcpServer->listen(QHostAddress::Any,port)){
        qDebug()<<"The port is already used";
    }
    connect(tcpServer, SIGNAL(newConnection()), this, SLOT(handleConnection()));
    std::cout<<"The Snarl Network Protokoll is developed for Snarl <http://www.fullphat.net/>"<<std::endl;
}

SnarlNetworkFrontend::~SnarlNetworkFrontend(){
    delete parser;
    delete tcpServer;
}


void SnarlNetworkFrontend::actionInvoked(QSharedPointer<Notification>notification){
    //TODO:fix callback
    SnarlNotification sn=notifications.value(notification->id());
    if(notification->actionInvoked()==1)
        callback(sn,"SNP/1.1/304/Notification acknowledged/");
    else if(notification->actionInvoked()==2)
        callback(sn,"SNP/1.1/302/Notification cancelled/");
}
void SnarlNetworkFrontend::notificationClosed(QSharedPointer<Notification>notification){
    SnarlNotification sn=notifications.value(notification->id());
    if(notification->actionInvoked()==Notification::TIMED_OUT)
        callback(sn,"SNP/1.1/303/Notification timed out/");
    else
        callback(sn,"SNP/1.1/307/Notification closed/");
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
        notifications.insert(noti.notification->id(),noti);
        if(!noti.vailid)
            continue;
        if(noti.notification->isNotification()){
            snore()->broadcastNotification(noti.notification);
            if(noti.notification->id()!=0){
                out+="/"+QString::number(noti.notification->id())+"\r\n";
            }
        }else{
            out+="\r\n";
        }
        client->write(out.toLatin1());
        if(noti.httpClient){
            client->disconnectFromHost();
            client->waitForDisconnected();
        }
        qDebug()<<out;
    }
}

void SnarlNetworkFrontend::callback(const SnarlNotification &sn,QString msg){
    notifications.remove(sn.notification->id());
    if(sn.clientSocket!=NULL&&!msg.isEmpty()){
        msg+=QString::number(sn.notification->id());
        qDebug()<<msg;
        sn.clientSocket->write(msg.toAscii()+"\r\n");
        sn.clientSocket->flush();

        if(sn.httpClient){
            sn.clientSocket->waitForBytesWritten(-1);
            sn.clientSocket->disconnectFromHost();
        }
    }
}


#include "snarlnetwork.moc"
