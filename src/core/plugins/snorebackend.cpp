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

#include "snorebackend.h"
#include "../snore.h"
#include "../snore_p.h"
#include "../application.h"
#include "../notification/notification.h"
#include "../notification/notification_p.h"

#include <QTimer>
#include <QDir>
#include <QDebug>


using namespace Snore;

SnoreBackend::SnoreBackend (const QString &name , bool canCloseNotification, bool supportsRichtext, bool canUpdateNotifications) :
    SnorePlugin ( name ),
    m_canCloseNotification(canCloseNotification),
    m_supportsRichtext(supportsRichtext),
    m_canUpdateNotification(canUpdateNotifications)
{

}

SnoreBackend::~SnoreBackend()
{
    snoreDebug( SNORE_DEBUG )<<"Deleting"<<name();
}


bool SnoreBackend::initialize( SnoreCore *snore )
{
    if(!SnorePlugin::initialize(snore))
    {
        return false;
    }

    connect( snore->d(), SIGNAL(applicationRegistered(const Snore::Application&)), this, SLOT(slotRegisterApplication(const Snore::Application&)));
    connect( snore->d(), SIGNAL(applicationDeregistered(const Snore::Application&)), this, SLOT(slotDeregisterApplication(const Snore::Application&)));

    connect( this, SIGNAL(notificationClosed(Snore::Notification)), snore->d(), SLOT(slotNotificationClosed(Snore::Notification)));
    connect( snore->d(), SIGNAL(notify(Snore::Notification)), this, SLOT(slotNotify(Snore::Notification)));

    foreach(const Application &a,snore->aplications())
    {
        this->slotRegisterApplication(a);
    }

    return true;
}


void SnoreBackend::requestCloseNotification (Notification notification, Notification::CloseReasons reason )
{
    if(canCloseNotification() && notification.isValid())
    {
        closeNotification(notification,reason);
        slotCloseNotification(notification);
    }
}

void SnoreBackend::closeNotification(Notification n, Notification::CloseReasons reason)
{
    if(!n.isValid())
    {
        return;
    }
    if(m_activeNotifications.contains(n.id()))
    {
        m_activeNotifications.remove(n.id());
    }
    if(n.isUpdate() && m_activeNotifications.contains(n.old().id()))
    {
        m_activeNotifications.remove(n.old().id());
    }
    n.data()->setCloseReason(reason);
    snoreDebug( SNORE_DEBUG ) << n;
    emit notificationClosed(n);
}

void SnoreBackend::setSupportsRichtext(bool b)
{
    m_supportsRichtext = b;
}

void SnoreBackend::slotCloseNotification(Notification notification)
{
    Q_UNUSED(notification)
}

SnoreSecondaryBackend::SnoreSecondaryBackend(const QString &name, bool supportsRhichtext):
    SnorePlugin(name),
    m_supportsRichtext(supportsRhichtext)
{

}

SnoreSecondaryBackend::~SnoreSecondaryBackend()
{
    snoreDebug( SNORE_DEBUG )<<"Deleting"<<name();
}

bool SnoreSecondaryBackend::supportsRichtext()
{
    return m_supportsRichtext;
}

Snore::Notification SnoreBackend::getActiveNotificationByID(uint id)
{
    return m_activeNotifications.value(id);
}

bool SnoreBackend::canCloseNotification() const
{
    return m_canCloseNotification;
}

bool SnoreBackend::canUpdateNotification() const
{
    return m_canUpdateNotification;
}

bool SnoreBackend::supportsRichtext() const
{
    return m_supportsRichtext;
}

void SnoreBackend::slotRegisterApplication(const Application &application)
{
    Q_UNUSED(application);
}

void SnoreBackend::slotDeregisterApplication(const Application &application)
{
    Q_UNUSED(application);
}

void SnoreBackend::addActiveNotification(Notification n)
{
    m_activeNotifications.insert(n.id(), n);
}


bool SnoreBackend::deinitialize()
{
    if(SnorePlugin::deinitialize())
    {
        foreach(Notification n,m_activeNotifications)
        {
            requestCloseNotification(n, Notification::DISMISSED);
        }


        foreach(const Application &a,snore()->aplications())
        {
            slotDeregisterApplication(a);
        }
        disconnect( snore()->d(), SIGNAL(applicationRegistered(const Snore::Application&)), this, SLOT(slotRegisterApplication(const Snore::Application&)));
        disconnect( snore()->d(), SIGNAL(applicationDeregistered(const Snore::Application&)), this, SLOT(slotDeregisterApplication(const Snore::Application&)));

        disconnect( this, SIGNAL(notificationClosed(Snore::Notification)), snore()->d(), SLOT(slotNotificationClosed(Snore::Notification)));
        disconnect( snore()->d(), SIGNAL(notify(Snore::Notification)), this, SLOT(slotNotify(Snore::Notification)));
        return true;
    }
    return false;
}


void SnoreBackend::startTimeout(Notification &notification)
{
    if(notification.isSticky())
    {
        return;
    }
    QTimer *timer = notification.data()->timeoutTimer();
    timer->stop();
    if(notification.isUpdate())
    {
        notification.old().data()->timeoutTimer()->stop();
    }
    timer->setInterval(notification.timeout() * 1000);
    connect(timer,SIGNAL(timeout()),this,SLOT(notificationTimedOut()));
    timer->start();
}

void SnoreBackend::notificationTimedOut()
{

    QTimer *timer = qobject_cast<QTimer*>(sender());
    Notification n = snore()->getActiveNotificationByID(timer->property("notificationID").toUInt());
    if(n.isValid())
    {
        snoreDebug( SNORE_DEBUG ) << n;
        snore()->requestCloseNotification(n,Notification::TIMED_OUT);
    }
}
