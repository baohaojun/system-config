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

#include "snoreserver.h"
#include  <QDebug>
#include "notification.h"
#include <QPluginLoader>
#include <iostream>


QString const SnoreServer::snoreTMP=QDir::temp().path()+"/SnoreNotify/";

SnoreServer::SnoreServer():primaryNotificationBackend(0)
{    qDebug()<<"Inititalized";
    QDir home(snoreTMP);
    if(home.exists()){
        QStringList filetypes;
        filetypes<<"*.png"<<"*.jpg";
        QStringList toDell;
        toDell=home.entryList(filetypes);
        foreach(QString s,toDell){
            home.remove(s);
        }

    }else
        QDir::temp().mkpath("SnoreNotify");
}

void SnoreServer::publicatePlugin(QObject *plugin){
    qDebug()<<"Loading plugin: "<<plugin->property("name").value<QString>();


    SnorePlugin *sp=qobject_cast<SnorePlugin*>(plugin);
    if(sp){
        plugins.insert(plugin->property("name").value<QString>(),plugin);
        qDebug()<<plugin->property("name").value<QString>()<<"is a SnorePlugin";
        sp->setSnore(this);
    }
    Notification_Frontend *nf=qobject_cast<Notification_Frontend*>(plugin);
    if(nf){
        qDebug()<<plugin->property("name").value<QString>()<<"is a Notification_Frontend";
        nf->setSnore(this);

    }

    Notification_Backend * nb=qobject_cast<Notification_Backend *>(plugin);
    if(nb){
        qDebug()<<plugin->property("name").value<QString>()<<"is a Notification_Backend";
        if(nb->isPrimaryNotificationBackend()){
            if(primaryNotificationBackend){
                notyfier.append(primaryNotificationBackend);
                connect(this,SIGNAL(notify(QSharedPointer<Notification>)),primaryNotificationBackend,SLOT(notify(QSharedPointer<Notification>)));

            }
            primaryNotificationBackend=nb;
        }else{
            notyfier.append(nb);
            connect(this,SIGNAL(notify(QSharedPointer<Notification>)),nb,SLOT(notify(QSharedPointer<Notification>)));
        }
        connect(this,SIGNAL(closeNotify(int)),nb,SLOT(closeNotification(int)));
        nb->setSnore(this);
    }
}

int SnoreServer::broadcastNotification(QSharedPointer<Notification> notification){
    emit notify(notification);
    qDebug()<<"Broadcasting notification:"<<notification->toSnalrString();
    if(primaryNotificationBackend!=NULL){
        notification->id=primaryNotificationBackend->notify(notification);
        std::cout<<"Notification ID: "<<QString::number(notification->id).toLatin1().data()<<std::endl;
        return  notification->id;
    }
    return -1;
}

void SnoreServer::closeNotification(QSharedPointer<Notification> notification){
    emit closeNotify(notification->id);
    Notification_Frontend *nf= qobject_cast<Notification_Frontend*>(plugins.value(notification->source));
    if(nf!=0){
        nf->notificationClosed(notification);
    }
}

void SnoreServer::notificationActionInvoked(QSharedPointer<Notification> notification){
    Notification_Frontend *nf= qobject_cast<Notification_Frontend*>(plugins.value(notification->source));
    if(nf!=0){
        nf->actionInvoked(notification);
    }
}

void SnoreServer::addApplication(QSharedPointer<Application> application){
    applications.insert(application->name,application);
    emit applicationListChanged();
}


bool SnoreServer::applicationListAlertIsActive(const QString &applicationName,const QString &alertName){
    return applications.contains(applicationName)&&applications.value(applicationName)->alerts.contains(alertName)
            &&!applications.value(applicationName)->alerts.value(alertName)->active;
}

void SnoreServer::addAlert(const QString &appName,const QString &alertName, const QString &alertTitle){
    applications.value(appName)->addAlert(alertName,alertTitle);
    emit applicationListChanged();
}

void SnoreServer::removeApplication(const QString& appName){
    applications.take(appName).clear();
    emit applicationListChanged();
}

#include "snoreserver.moc"
