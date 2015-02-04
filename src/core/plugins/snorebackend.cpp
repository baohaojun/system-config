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
#include <QThread>
#include <QMetaMethod>

using namespace Snore;

SnoreBackend::SnoreBackend(const QString &name , bool canCloseNotification, bool supportsRichtext, bool canUpdateNotifications) :
    SnorePlugin(name),
    m_canCloseNotification(canCloseNotification),
    m_supportsRichtext(supportsRichtext),
    m_canUpdateNotification(canUpdateNotifications)
{

}

SnoreBackend::~SnoreBackend()
{
    snoreDebug(SNORE_DEBUG) << "Deleting" << name();
}

bool SnoreBackend::initialize()
{
    if (!SnorePlugin::initialize()) {
        return false;
    }

    connect(SnoreCorePrivate::instance(), SIGNAL(applicationRegistered(Snore::Application)), this, SLOT(slotRegisterApplication(Snore::Application)), Qt::QueuedConnection);
    connect(SnoreCorePrivate::instance(), SIGNAL(applicationDeregistered(Snore::Application)), this, SLOT(slotDeregisterApplication(Snore::Application)), Qt::QueuedConnection);

    connect(this, SIGNAL(notificationClosed(Snore::Notification)), SnoreCorePrivate::instance(), SLOT(slotNotificationClosed(Snore::Notification)), Qt::QueuedConnection);
    connect(SnoreCorePrivate::instance(), SIGNAL(notify(Snore::Notification)), this, SLOT(slotNotify(Snore::Notification)), Qt::QueuedConnection);

    for(const Application & a : SnoreCore::instance().aplications()) {
        this->slotRegisterApplication(a);
    }

    return true;
}

void SnoreBackend::requestCloseNotification(Notification notification, Notification::CloseReasons reason)
{
    if (canCloseNotification() && notification.isValid()) {
        closeNotification(notification, reason);
        slotCloseNotification(notification);
    }
}

void SnoreBackend::closeNotification(Notification n, Notification::CloseReasons reason)
{
    if (!n.isValid()) {
        return;
    }
    if (m_activeNotifications.contains(n.id())) {
        m_activeNotifications.remove(n.id());
    }
    if (n.isUpdate() && m_activeNotifications.contains(n.old().id())) {
        m_activeNotifications.remove(n.old().id());
    }
    n.data()->setCloseReason(reason);
    snoreDebug(SNORE_DEBUG) << n;
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
    snoreDebug(SNORE_DEBUG) << "Deleting" << name();
}

bool SnoreSecondaryBackend::initialize()
{
    if (!SnorePlugin::initialize()) {
        return false;
    }
    connect(SnoreCorePrivate::instance(), SIGNAL(notify(Snore::Notification)), this, SLOT(slotNotify(Snore::Notification)), Qt::QueuedConnection);
    return true;
}

bool SnoreSecondaryBackend::deinitialize()
{
    if (SnorePlugin::deinitialize()) {
        disconnect(SnoreCorePrivate::instance(), SIGNAL(notify(Snore::Notification)), this, SLOT(slotNotify(Snore::Notification)));
        return true;
    }
    return false;
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
    if (SnorePlugin::deinitialize()) {
        foreach(Notification n, m_activeNotifications) {
            requestCloseNotification(n, Notification::DISMISSED);
        }

        foreach(const Application & a, snore()->aplications()) {
            slotDeregisterApplication(a);
        }
        disconnect(SnoreCorePrivate::instance(), SIGNAL(applicationRegistered(Snore::Application)), this, SLOT(slotRegisterApplication(Snore::Application)));
        disconnect(SnoreCorePrivate::instance(), SIGNAL(applicationDeregistered(Snore::Application)), this, SLOT(slotDeregisterApplication(Snore::Application)));

        disconnect(this, SIGNAL(notificationClosed(Snore::Notification)), SnoreCorePrivate::instance(), SLOT(slotNotificationClosed(Snore::Notification)));
        disconnect(SnoreCorePrivate::instance(), SIGNAL(notify(Snore::Notification)), this, SLOT(slotNotify(Snore::Notification)));
        return true;
    }
    return false;
}

void SnoreBackend::startTimeout(Notification &notification)
{
    if (thread() != QThread::currentThread()) {
        metaObject()->invokeMethod(this, "startTimeout", Qt::QueuedConnection, Q_ARG(Notification, notification));
        return;
    }
    if (notification.isSticky()) {
        return;
    }
    QTimer *timer = notification.data()->timeoutTimer();
    timer->stop();
    if (notification.isUpdate()) {
        notification.old().data()->timeoutTimer()->stop();
    }
    timer->setInterval(notification.timeout() * 1000);
    connect(timer, SIGNAL(timeout()), this, SLOT(notificationTimedOut()), Qt::QueuedConnection);
    timer->start();
}

void SnoreBackend::notificationTimedOut()
{

    QTimer *timer = qobject_cast<QTimer *>(sender());
    Notification n = SnoreCore::instance().getActiveNotificationByID(timer->property("notificationID").toUInt());
    if (n.isValid()) {
        snoreDebug(SNORE_DEBUG) << n;
        SnoreCore::instance().requestCloseNotification(n, Notification::TIMED_OUT);
    }
}
