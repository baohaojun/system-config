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
#include "notification/notification.h"
#include "trayiconnotifer.h"

#include <iostream>

#include <QPluginLoader>
#include <QDebug>
#include <QDir>
#include <QSystemTrayIcon>


QString const SnoreServer::version(){
    return "0.15";
}

QString const SnoreServer::snoreTMP(){
    static const QString tmp = QDir::temp().path() +"/SnoreNotify/";
    return tmp;
}


SnoreServer::SnoreServer ( QSystemTrayIcon *trayIcon ) :
        _notificationBackend ( NULL ),
        _trayIcon ( trayIcon )
{    
    QDir home ( snoreTMP() );
    if ( !home.exists() ){
        home.cdUp();
        home.mkdir("SnoreNotify");
    }

    if ( trayIcon!=NULL )
    {
        publicatePlugin ( new TrayIconNotifer ( this,trayIcon ) );
    }

}
void SnoreServer::publicatePlugin ( const QString &fileName )
{
    QPluginLoader loader ( fileName );
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

    plugins.insert ( pluginName,plugin );
    qDebug() <<pluginName<<"is a SnorePlugin";
    plugin->setSnore ( this );

    Notification_Backend * nb=qobject_cast<Notification_Backend *> ( plugin );
    if ( nb )
    {
        qDebug() <<pluginName<<"is a Notification_Backend";
        if ( nb->isPrimaryNotificationBackend() )
        {
            _primaryNotificationBackends.append( pluginName);
            if ( _notificationBackend == NULL )
            {
                _notificationBackend = nb;
                qDebug() <<"Primary NotificationBackend is"<<nb->name();
            }
        }
        else
        {
            connect ( this,SIGNAL ( notify (Notification) ),nb,SLOT ( notify ( Notification ) ) );
        }
        _notyfier.insert ( pluginName,nb );

        connect ( this,SIGNAL ( closeNotify ( Notification ) ),nb,SLOT ( closeNotification ( Notification) ) );
        connect ( this,SIGNAL ( applicationInitialized ( Application* ) ),nb,SLOT ( registerApplication ( Application* ) ) );
        connect ( this,SIGNAL ( applicationRemoved ( Application* ) ),nb,SLOT ( unregisterApplication ( Application* ) ) );
    }
}

int SnoreServer::broadcastNotification ( Notification notification )
{
    emit notify ( notification );
    if ( _notificationBackend!=NULL )
    {
        notification.setId(_notificationBackend->notify ( notification ));
        return  notification.id();
    }
    return -1;
}

void SnoreServer::closeNotification ( Notification notification,const NotificationEnums::CloseReasons::closeReasons &reason )
{
    notification.setCloseReason(reason);
    emit closeNotify ( notification );
    Notification_Frontend *nf= notification.source();
    if ( nf != NULL )
    {
        nf->notificationClosed ( notification );
    }
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
    emit applicationRemoved ( _applications.value ( appName ) );
    _applications.take ( appName )->deleteLater();
}

const ApplicationsList &SnoreServer::aplications() const
{
    return _applications;
}


const QStringList &SnoreServer::primaryNotificationBackends() const
{
    return _primaryNotificationBackends;
}

void SnoreServer::setPrimaryNotificationBackend ( const QString &backend )
{
    if(!_primaryNotificationBackends.contains(backend))
        return;
    qDebug()<<"Setting Notification Backend to:"<<backend;
    _notificationBackend = qobject_cast<Notification_Backend*>(plugins[backend]);
}

const QString &SnoreServer::primaryNotificationBackend(){
    return _notificationBackend->name();
}


#include "snoreserver.moc"
