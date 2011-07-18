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

#include "snorenotificationinstance.h"

#include <QDebug>

SnoreNotificationInstance::SnoreNotificationInstance ( const QString &appname, SnoreServer *parent,const QString &icon ) :
        Notification_Frontend(appname,parent),
        _app ( new Application ( appname ,icon) ),
        _snore ( parent )
{
    setParent ( parent );
}

SnoreNotificationInstance::~SnoreNotificationInstance()
{
    unregisterWithBackends();
}


void SnoreNotificationInstance::addAlert (const QString &name, const QString &title, const QString &icon)
{
    _app->addAlert ( new Alert ( name,title.isNull() ?name:title,icon )  );

}

void SnoreNotificationInstance::registerWithBackends()
{
    _snore->addApplication ( _app );
    _snore->applicationIsInitialized ( _app );

}

void SnoreNotificationInstance::unregisterWithBackends()
{
    _snore->removeApplication ( _app->name() );
}

int SnoreNotificationInstance::notify ( const QString &alert, const QString &title, const QString &text, const QString &icon, int timeout,Notification::prioritys priority )
{
    return _snore->broadcastNotification (  Notification( this,_app->name(),alert,title,text,icon,timeout,0,priority ) );
}

void SnoreNotificationInstance::actionInvoked ( Notification notification ){
    emit notificationActionInvoked(notification);
}

void SnoreNotificationInstance::notificationClosed ( Notification notification ){

}

#include "snorenotificationinstance.moc"
