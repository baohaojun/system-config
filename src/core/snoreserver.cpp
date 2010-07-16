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
#include "notification.h"
#include <iostream>

#include <QPluginLoader>
#include <QDebug>
#include <QDir>
#include <QSystemTrayIcon>

QString const SnoreServer::snoreTMP=QDir::temp().path()+"/SnoreNotify/";

SnoreServer::SnoreServer(QSystemTrayIcon *trayIcon):
        _notificationBackend(0),
        _trayIcon(trayIcon)
{
    qDebug()<<"Inititalized";
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

void SnoreServer::publicatePlugin(const QString &fileName){
    QPluginLoader loader(fileName);
    QObject *plugin = loader.instance();
    if (plugin==NULL) {
        qDebug()<<"Failed loading plugin: "<<loader.errorString();
        return;
    }

    SnorePlugin *sp = qobject_cast<SnorePlugin*>(plugin);
    if(sp==NULL){
        qDebug()<<"Error:"<<fileName<<"is not a snarl plugin" ;
        return;
    }
    QString pluginName(sp->name());

    qDebug()<<"Loading plugin: "<<pluginName;

    plugins.insert(pluginName,sp);
    qDebug()<<pluginName<<"is a SnorePlugin";
    sp->setSnore(this);

    Notification_Frontend *nf=qobject_cast<Notification_Frontend*>(plugin);
    if(nf){
        qDebug()<<pluginName<<"is a Notification_Frontend";
        nf->setSnore(this);

    }

    Notification_Backend * nb=qobject_cast<Notification_Backend *>(plugin);
    if(nb){
        qDebug()<<pluginName<<"is a Notification_Backend";
        if(nb->isPrimaryNotificationBackend()){
            if(_notificationBackend){
                _notyfier.insert(pluginName,nb);
                _primaryNotificationBackends.insert(pluginName,nb);
                connect(this,SIGNAL(notify(QSharedPointer<Notification>)),_notificationBackend,SLOT(notify(QSharedPointer<Notification>)));
            }
            _notificationBackend=nb;
            _notificationBackend->notify(QSharedPointer<Notification>(new Notification(NULL,"Welcome","Snore Notify succesfully registred "+plugin->property("name").value<QString>(),"")));

        }else{
            _notyfier.insert(pluginName,nb);
            connect(this,SIGNAL(notify(QSharedPointer<Notification>)),nb,SLOT(notify(QSharedPointer<Notification>)));
        }
        connect(this,SIGNAL(closeNotify(int)),nb,SLOT(closeNotification(int)));
        nb->setSnore(this);
    }
}

int SnoreServer::broadcastNotification(QSharedPointer<Notification> notification){
    emit notify(notification);
    qDebug()<<"Broadcasting notification:"<<notification->toString();
    if(_notificationBackend!=NULL){
        notification->_id=_notificationBackend->notify(notification);
        std::cout<<"Notification ID: "<<QString::number(notification->_id).toLatin1().data()<<std::endl;
        return  notification->_id;
    }
    return -1;
}

void SnoreServer::closeNotification(QSharedPointer<Notification> notification){
    emit closeNotify(notification->_id);
    Notification_Frontend *nf= notification->_source;
    if(nf!=0){
        nf->notificationClosed(notification);
    }
}

void SnoreServer::notificationActionInvoked(QSharedPointer<Notification> notification){
    Notification_Frontend *nf= notification->_source;
    if(nf!=0){
        nf->actionInvoked(notification);
    }
}

void SnoreServer::addApplication(QSharedPointer<Application> application){
    _applications.insert(application->name,application);
    emit applicationListChanged();
}


bool SnoreServer::applicationListAlertIsActive(const QString &applicationName,const QString &alertName){
    return _applications.contains(applicationName)&&_applications.value(applicationName)->alerts.contains(alertName)
            &&!_applications.value(applicationName)->alerts.value(alertName)->active;
}

void SnoreServer::addAlert(const QString &appName,const QString &alertName, const QString &alertTitle){
    _applications.value(appName)->addAlert(alertName,alertTitle);
    emit applicationListChanged();
}

void SnoreServer::removeApplication(const QString& appName){
    _applications.take(appName).clear();
    emit applicationListChanged();
}

const ApplicationsList &SnoreServer::aplicationList() const{
    return _applications;
}

const QHash<QString,Notification_Backend*> &SnoreServer::primaryNotificationBackends()const{
    return _primaryNotificationBackends;
}

void SnoreServer::setNotificationBackend(Notification_Backend *backend){
    _notificationBackend=backend;
}

#include "snoreserver.moc"
