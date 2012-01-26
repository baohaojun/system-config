/****************************************************************************************
 * Copyright (c) 2010-2012 Patrick von Reth <patrick.vonreth@gmail.com>                 *
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
#include "notification/notification.h"
#include "trayiconnotifer.h"
#include "version.h"

#include <iostream>

#include <QPluginLoader>
#include <QDebug>
#include <QDir>
#include <QSystemTrayIcon>
#include <QApplication>

namespace Snore{

QString const SnoreServer::version(){
    return QString().append(Version::major()).append(".").append(Version::minor()).append(Version::suffix());
}

QString const SnoreServer::snoreTMP(){
    static const QString tmp = QDir::temp().path() +"/SnoreNotify/";
    return tmp;
}


SnoreServer::SnoreServer ( QSystemTrayIcon *trayIcon ) :
        m_notificationBackend ( NULL ),
        m_trayIcon ( trayIcon )
{    
    QDir home ( snoreTMP() );
    if ( !home.exists() ){
        home.cdUp();
        home.mkdir("SnoreNotify");
    }

    if ( trayIcon!=NULL )
    {
        publicatePlugin ( new TrayIconNotifer ( trayIcon ) );
    }

}
void SnoreServer::publicatePlugin ( const QString &fileName )
{
    QPluginLoader loader ( fileName );
    qDebug()<<"Trying to load"<<fileName;
    if ( !loader.load())
    {
        qDebug() <<"Failed loading plugin: "<<loader.errorString();
        return;
    }
    SnorePlugin *sp = qobject_cast<SnorePlugin*> ( loader.instance());
    if ( sp == NULL )
    {
        std::cerr<<"Error:"<<fileName.toLatin1().data() <<" is not a snarl plugin"<<std::endl ;
        return;
    }
    publicatePlugin ( sp );
}

void SnoreServer::publicatePlugin ( SnorePlugin *plugin )
{


    QString pluginName ( plugin->name() );

    qDebug() <<"Loading plugin: "<<pluginName;

    m_plugins.insert ( pluginName,plugin );
    qDebug() <<pluginName<<"is a SnorePlugin";

    Notification_Backend * nb = qobject_cast<Notification_Backend *> ( plugin );
    if ( nb )
    {
        qDebug() <<pluginName<<"is a Notification_Backend";
        if ( nb->isPrimaryNotificationBackend() )
        {
            m_primaryNotificationBackends.append( pluginName);
            if ( m_notificationBackend == NULL )
            {
                m_notificationBackend = nb;
            }
        }else{
            if(!nb->init( this )){
                nb->deleteLater();
                return;
            }
        }
        m_notyfier.insert ( pluginName,nb );

    }else{
         Notification_Frontend * nf = qobject_cast<Notification_Frontend*> ( plugin );
         if(nf != NULL){
             qDebug() <<pluginName<<"is a Notification_Frontend";
            if(nf->init( this ))
                m_frontends.insert(nf->name(),nf);
            else
                nf->deleteLater();
         }
    }
}

uint SnoreServer::broadcastNotification ( Notification notification )
{
    qDebug()<<"Broadcasting"<<notification.title()<<notification.timeout();
    emit notify ( notification );
    if ( m_notificationBackend != NULL )
    {
        if(!m_notificationBackend->isInitialized()){
            qDebug()<<"Notification backend "<<m_notificationBackend<<" isnt initialized will snore will exit now";
            qApp->quit();
        }
        notification.setId(m_notificationBackend->notify( notification ));
        return  notification.id();
    }
    return -1;
}

void SnoreServer::closeNotification ( Notification notification,const NotificationEnums::CloseReasons::closeReasons &reason )
{
    notification.setCloseReason(reason);
    emit closeNotify ( notification );
}

void SnoreServer::notificationActionInvoked ( Notification notification )
{
    emit actionInvoked(notification);
    Notification_Frontend *nf= notification.source();
    if ( nf != NULL )
    {
        nf->actionInvoked ( notification );
    }
}

void SnoreServer::addApplication ( Application *application )
{
    _applications.insert ( application->name(),application );
}

void SnoreServer::applicationIsInitialized ( Application *application )
{
    application->setInitialized ( true );
    emit applicationInitialized ( application );
}

void SnoreServer::removeApplication ( const QString& appName )
{
    qDebug()<<"Remove Application"<<appName;
    emit applicationRemoved ( _applications.value ( appName ) );
    _applications.take ( appName )->deleteLater();
}

const ApplicationsList &SnoreServer::aplications() const
{
    return _applications;
}


const QStringList &SnoreServer::primaryNotificationBackends() const
{
    return m_primaryNotificationBackends;
}

void SnoreServer::setPrimaryNotificationBackend ( const QString &backend )
{
    if(!m_primaryNotificationBackends.contains(backend))
        return;
    qDebug()<<"Setting Notification Backend to:"<<backend;
    m_notificationBackend = qobject_cast<Notification_Backend*>(m_plugins[backend]);
    if(!m_notificationBackend->isInitialized())
        m_notificationBackend->init(this);
}

const QString &SnoreServer::primaryNotificationBackend(){
    return m_notificationBackend->name();
}

}

#include "snoreserver.moc"
