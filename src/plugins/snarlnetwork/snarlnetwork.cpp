#include "snarlnetwork.h"
#include "core/snoreserver.h"
#include <QtCore>
#include <QTcpServer>


Q_EXPORT_PLUGIN2(snalnetwork,SnarlNetworkFrontend)

SnarlNetworkFrontend::SnarlNetworkFrontend():parser(this){
    setProperty("name","SnarlNetworkFrontend");
    tcpServer=new QTcpServer(this);
    if(!tcpServer->listen(QHostAddress::Any,port)){
        qDebug()<<"The port is already used";
    }
    connect(tcpServer, SIGNAL(newConnection()), this, SLOT(handleConnection()));
}


void SnarlNetworkFrontend::actionInvoked(QSharedPointer<Notification>notification){
    //TODO:fix callback
    SnarlNotification sn=notifications.value(notification->getID());
    if(notification->actionInvoked==1)
        callback(sn,"SNP/1.1/304/Notification acknowledged/");
    else if(notification->actionInvoked==2)
        callback(sn,"SNP/1.1/302/Notification cancelled/");
}
void SnarlNetworkFrontend::notificationClosed(QSharedPointer<Notification>notification){
    SnarlNotification sn=notifications.value(notification->getID());
    if(notification->actionInvoked==Notification::TIMED_OUT)
        callback(sn,"SNP/1.1/303/Notification timed out/");
    else
        callback(sn,"SNP/1.1/307/Notification closed/");
}

void SnarlNetworkFrontend::handleConnection(){
    QTcpSocket *client = tcpServer->nextPendingConnection();
    connect(client,SIGNAL(readyRead()),this,SLOT(handleMessages()));
    connect(client,SIGNAL(disconnected()), this, SLOT(deleteLater()));
}

void SnarlNetworkFrontend::handleMessages(){
    QString out("SNP/1.1/0/OK");
    QTcpSocket *client=qobject_cast<QTcpSocket*>(sender());
    QStringList incommings(QString::fromUtf8(client->readAll()).split("\r\n"));
    foreach(QString s,incommings){
        SnarlNotification noti=parser.parse(s,client);
        notifications.insert(noti.notification->getID(),noti);
        if(!noti.vailid)
            continue;
        if(noti.notification->isNotification()){
            getSnore()->broadcastNotification(noti.notification);
            if(noti.notification->getID()!=0){
                out+="/"+QString::number(noti.notification->getID())+"\r\n";
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

void SnarlNetworkFrontend::clientDisconnecd(){
    QTcpSocket *client=(QTcpSocket*)sender();
    client->deleteLater();
}

void SnarlNetworkFrontend::callback(const SnarlNotification &sn,QString msg){
    notifications.remove(sn.notification->getID());
    if(sn.clientSocket!=NULL&&!msg.isEmpty()){
        msg+=QString::number(sn.notification->getID());
        qDebug()<<msg;
        sn.clientSocket->write(msg.toAscii()+"\n");
        sn.clientSocket->flush();

        if(sn.httpClient){
            sn.clientSocket->waitForBytesWritten(-1);
            sn.clientSocket->disconnectFromHost();
        }
    }
}



#include "snarlnetwork.moc"
