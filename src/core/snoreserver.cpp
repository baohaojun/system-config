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
#include "trayiconnotifer.h"
#include "snorenotificationinstance.h"

#include <iostream>

#include <QPluginLoader>
#include <QDebug>
#include <QDir>
#include <QSystemTrayIcon>


QString const SnoreServer::version(){
    return "0.1";
}

QString const SnoreServer::snoreTMP(){
    static const QString tmp = QDir::temp().path() +"/SnoreNotify/";
    return tmp;
}

void SnoreServer::cleanupTMP(){
    QDir home ( snoreTMP() );
    if ( home.exists() )
    {
        QStringList filetypes;
        filetypes<<"*.png"<<"*.jpg";
        QStringList toDell;
        toDell=home.entryList ( filetypes );
        foreach ( QString s,toDell )
        {
            home.remove ( s );
        }

    }
    else{
        home.cdUp();
        home.mkdir("SnoreNotify");
    }

}

SnoreServer::SnoreServer ( QSystemTrayIcon *trayIcon ) :
        _notificationBackend ( NULL ),
        _trayIcon ( trayIcon )
{
    qDebug() <<"Inititalized";



    _defaultNotificationInterface = new SnoreNotificationInstance ( "Snore",this );


    if ( trayIcon!=NULL )
    {
        publicatePlugin ( new TrayIconNotifer ( this,trayIcon ) );
    }

}
void SnoreServer::publicatePlugin ( const QString &fileName )
{
    QPluginLoader loader ( fileName );
    QObject *plugin = loader.instance();
    if ( plugin==NULL )
    {
        qDebug() <<"Failed loading plugin: "<<loader.errorString();
        return;
    }
    SnorePlugin *sp = dynamic_cast<SnorePlugin*> ( plugin );
    if ( sp==NULL )
    {
        std::cerr<<"Error:"<<fileName.toLatin1().data() <<"is not a snarl plugin"<<std::endl ;
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

    Notification_Frontend *nf=qobject_cast<Notification_Frontend*> ( plugin );
    if ( nf )
    {
        qDebug() <<pluginName<<"is a Notification_Frontend";
        nf->setSnore ( this );

    }

    Notification_Backend * nb=qobject_cast<Notification_Backend *> ( plugin );
    if ( nb )
    {
        nb->setSnore ( this );
        qDebug() <<pluginName<<"is a Notification_Backend";
        if ( nb->isPrimaryNotificationBackend() )
        {
            _primaryNotificationBackends.insert ( pluginName,nb );
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
        qDebug()<<"Broadcasting";
        notification.setId(_notificationBackend->notify ( notification ));
        qDebug()<<"Notification ID: "<<notification.id();
        return  notification.id();
    }
    return -1;
}

void SnoreServer::closeNotification ( Notification notification,const Notification::closeReasons &reason )
{
	qDebug()<<"closing notification"<<notification.id()<<"reason"<<reason;
	notification.setCloseReason(reason);
    emit closeNotify ( notification );
    Notification_Frontend *nf= notification.source();
    if ( nf != 0 )
    {
        nf->notificationClosed ( notification );
    }
}

void SnoreServer::notificationActionInvoked ( Notification notification )
{
    Notification_Frontend *nf= notification.source();
    if ( nf!=0 )
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


const QHash<QString,Notification_Backend*> &SnoreServer::primaryNotificationBackends() const
{
    return _primaryNotificationBackends;
}

void SnoreServer::setPrimaryNotificationBackend ( Notification_Backend *backend )
{
    if(!backend->isPrimaryNotificationBackend())
        return;
    qDebug()<<"Setting Notification Backend to:"<<backend->name();
    _notificationBackend = backend;
}

Notification_Backend * SnoreServer::primaryNotificationBackend(){
    return _notificationBackend;
}

SnoreNotificationInstance * SnoreServer::defaultNotificationInterface()
{
    return _defaultNotificationInterface;
}

#include "snoreserver.moc"
