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

#include "snorebackend.h"
#include "../snore.h"
#include "../application.h"
#include "../notification/notification.h"

#include <QTimer>
#include <QPluginLoader>
#include <QDir>
#include <QDebug>

namespace Snore{

SnoreBackend::SnoreBackend ( const QString &name ) :
    SnorePlugin ( name )
{

}

SnoreBackend::~SnoreBackend()
{
    qDebug()<<"Deleting"<<name();
}


bool SnoreBackend::init( SnoreCore *snore )
{
    if(!SnorePlugin::init(snore))
        return false;
    connect( snore,SIGNAL( applicationInitialized( Snore::Application* ) ),this,SLOT( slotRegisterApplication( Snore::Application* ) ) );
    connect( snore,SIGNAL( applicationRemoved( Snore::Application* ) ),this,SLOT( slotUnregisterApplication( Snore::Application* ) ) );

    foreach(Application *a,snore->aplications()){
        this->slotRegisterApplication(a);
    }

    return true;
}


bool SnoreBackend::requestCloseNotification ( Notification notification,NotificationEnums::CloseReasons::closeReasons reason )
{
    if(slotCloseNotification(notification))
    {
        notification.setCloseReason(reason);
        return true;
    }
    return false;
}

void SnoreBackend::closeNotification(Notification n, NotificationEnums::CloseReasons::closeReasons reason)
{
    m_activeNotifications.remove(n.id());
    n.setCloseReason(reason);
    emit closeNotification(n);
}

SnoreSecondaryBackend::SnoreSecondaryBackend(const QString &name)
    :SnoreBackend(name)
{

}

SnoreSecondaryBackend::~SnoreSecondaryBackend()
{
    qDebug()<<"Deleting"<<name();
}

bool SnoreSecondaryBackend::init(SnoreCore *snore)
{
    connect( snore,SIGNAL( slotNotify(SnoreCore::Notification) ),this,SLOT( slotNotify( SnoreCore::Notification ) ) );
    return SnoreBackend::init(snore);
}

Snore::Notification SnoreBackend::getActiveNotificationByID(uint id)
{
    return m_activeNotifications[id];
}

void SnoreBackend::addActiveNotification(Notification n)
{
    m_activeNotifications[n.id()] = n;
}


}
#include "snorebackend.moc"
