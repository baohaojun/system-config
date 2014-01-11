/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2014  Patrick von Reth <vonreth@kde.org>


    SnoreNotify is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SnoreNotify is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with SnoreNotify.  If not, see <http://www.gnu.org/licenses/>.
*/



#include "notification/notification_p.h"
#include "notification/icon.h"
#include "../hint.h"

#include <QSharedData>

using namespace Snore;

uint NotificationData::notificationCount = 0;

uint NotificationData::m_idCount = 1;

NotificationData::NotificationData ( const QString &application,const QString &alert,const QString &title,const QString &text,const Icon &icon,
                                     int timeout,NotificationEnums::Prioritys::prioritys priority ):
    m_id ( m_idCount++ ),
    m_updateID(0),
    m_timeout ( timeout ),
    m_source ( NULL),
    m_application ( application ),
    m_alert ( alert ),
    m_title ( title ),
    m_text ( text ),
    m_icon ( icon ),
    m_priority(priority),
    m_closeReason(NotificationEnums::CloseReasons::NONE),
    m_actionInvoked( NULL )
{
    notificationCount++;
    qDebug()<< "Creating Notification: ActiveNotifications" << notificationCount << "id" << m_id;
}

NotificationData::~NotificationData()
{
    notificationCount--;
    qDebug() << "Deleting Notification: ActiveNotifications" << notificationCount << "id" << m_id;
}


void NotificationData::setActionInvoked ( Notification::Action *action )
{
    m_actionInvoked = action;
}

void NotificationData::setActionInvoked ( const int &id)
{
    m_actionInvoked = m_actions[id];
}
