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

#include "redirector.h"

#include "core/snoreserver.h"
#include "core/notification.h"
#include "core/utils.h"

#include <QDebug>
#include <QHostAddress>
#include <QtCore>
#include <QObject>

Q_EXPORT_PLUGIN2(redircetor,Redircetor)



Redircetor::Redircetor(){
    setProperty("name","Redircetor");
    WebInterface::getInstance()->publicatePlugin(this);
    getArgument.insert("subscribe",SUBSCRIBE);
    getArgument.insert("unsubscribe",UNSUBSCRIBE);
    getArgument.insert("listsubscibers",LISTSUBSCRIBERS);
}


bool Redircetor::parseCommand(QTcpSocket *client, const QString &command){
    QHostAddress addres=client->peerAddress();
    QString out;
    qDebug()<<"parsing";
    qDebug()<<command;
    qDebug()<<getArgument.value(command);
    switch(getArgument.value(command)){
    case UNSUBSCRIBE:
        if(!subscribers.contains(addres.toString())){
            out=  "Unubscribe failed you are not a subscriber";
            break;
        }
        subscribers.take(addres.toString()).clear();
        out="Sucsefully unsuscribed";
        break;
    case SUBSCRIBE:{
            if(subscribers.contains(addres.toString())){out="Allready suscribed";break;}
            if(addres.toString()=="127.0.0.1"){out="Cant subscribe yourself";break;}
            QSharedPointer<QTcpSocket> subscriber(new QTcpSocket);
            subscriber->connectToHost(addres,port ,QTcpSocket::ReadWrite);
            if(subscriber->waitForConnected()){
                SnoreServer* snore(getSnore());
                foreach(QSharedPointer<Application> a,snore->getAplicationList()->values()){
                    QString* app=&a->name;
                    subscriber->write(QString("type=SNP#?version=1.1#?action=register#?app="+*app+"\r\n").toUtf8());
                    foreach(const QSharedPointer<Alert> al,a->alerts.values()){
                        subscriber->write(QString("type=SNP#?version=1.1#?action=add_class#?app="+*app+"#?class="+al->name+"#?title="+al->title+"\r\n").toUtf8());
                    }
                }
                subscribers.insert(addres.toString(),subscriber);
                out=addres.toString()+" Sucsefully suscribed";
            }else
                out="Subscription failed no client running?";
            break;
        }
    case  LISTSUBSCRIBERS:
        out+="\nRegistred Subscriber\n";
        foreach(const QString &s,subscribers.keys()){
            out+=s+"\n";
        }
        break;
        default:
        return false;
    }
    if(!out.isEmpty()){
        client->write(out.toUtf8());
        client->disconnectFromHost();
        client->waitForDisconnected();
        return true;
    }
    return false;
}
int Redircetor::notify(QSharedPointer<Notification>notification){    
    foreach(QSharedPointer<QTcpSocket> s,subscribers.values()){
        if(s->isWritable()){
            qDebug()<<"Sending to subscriber"<<s->peerAddress();
            s->write((Utils::notificationToSNTPString(notification)+"\r\n").toLatin1());
        }
    }
    return -1;
}
void Redircetor::closeNotification(int nr){
    qWarning()<<"Not supported";
}

QString Redircetor::display(){
    return "<a href=\"/subscribe\" >Subscribe </a><br><a href=\"/unsubscribe\" >Unsubscribe </a><br><br><a href=\"/listsubscribers\" >Prints a list of all subscribers</a><br>";
}

SnoreServer* Redircetor::getSnore(){
    return this->snore.data();
}
void Redircetor::setSnore(SnoreServer *snore){
    this->WebInterface_Plugin::setSnore(snore);
}


#include "redirector.moc"
