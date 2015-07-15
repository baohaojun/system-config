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

SnoreBackend::SnoreBackend()
{
    connect(this, &SnoreSecondaryBackend::enabledChanged, [this](bool enabled) {
        if (enabled) {
            connect(SnoreCorePrivate::instance(), &SnoreCorePrivate::applicationRegistered, this, &SnoreBackend::slotRegisterApplication, Qt::QueuedConnection);
            connect(SnoreCorePrivate::instance(), &SnoreCorePrivate::applicationDeregistered, this, &SnoreBackend::slotDeregisterApplication, Qt::QueuedConnection);

            connect(this, &SnoreBackend::notificationClosed, SnoreCorePrivate::instance(), &SnoreCorePrivate::slotNotificationClosed, Qt::QueuedConnection);
            connect(SnoreCorePrivate::instance(), &SnoreCorePrivate::notify, this, &SnoreBackend::slotNotify, Qt::QueuedConnection);

            for (const Application &a : SnoreCore::instance().aplications()) {
                slotRegisterApplication(a);
            }
        } else {
            for (const Application &a : SnoreCore::instance().aplications()) {
                slotDeregisterApplication(a);
            }
            disconnect(SnoreCorePrivate::instance(), &SnoreCorePrivate::applicationRegistered, this, &SnoreBackend::slotRegisterApplication);
            disconnect(SnoreCorePrivate::instance(), &SnoreCorePrivate::applicationDeregistered, this, &SnoreBackend::slotDeregisterApplication);

            disconnect(this, &SnoreBackend::notificationClosed, SnoreCorePrivate::instance(), &SnoreCorePrivate::slotNotificationClosed);
            disconnect(SnoreCorePrivate::instance(), &SnoreCorePrivate::notify, this, &SnoreBackend::slotNotify);
        }
    });
}

SnoreBackend::~SnoreBackend()
{
    snoreDebug(SNORE_DEBUG) << "Deleting" << name();
}

void SnoreBackend::requestCloseNotification(Notification notification, Notification::CloseReasons reason)
{
    if (notification.isValid()) {
        if (canCloseNotification()) {
            slotCloseNotification(notification);
            closeNotification(notification, reason);
        }
    }
}

void SnoreBackend::closeNotification(Notification n, Notification::CloseReasons reason)
{
    if (!n.isValid()) {
        return;
    }
    n.removeActiveIn(this);
    if (n.isUpdate()) {
        n.old().removeActiveIn(this);
    }
    n.data()->setCloseReason(reason);
    snoreDebug(SNORE_DEBUG) << n << reason;
    emit notificationClosed(n);
}

void SnoreBackend::slotCloseNotification(Notification notification)
{
    Q_UNUSED(notification)
}

SnoreSecondaryBackend::SnoreSecondaryBackend()
{
    connect(this, &SnoreSecondaryBackend::enabledChanged, [this](bool enabled) {
        if (enabled) {
            connect(SnoreCorePrivate::instance(), &SnoreCorePrivate::notify, this, &SnoreSecondaryBackend::slotNotify, Qt::QueuedConnection);
            connect(SnoreCorePrivate::instance(), &SnoreCorePrivate::notificationDisplayed, this, &SnoreSecondaryBackend::slotNotificationDisplayed, Qt::QueuedConnection);
        } else {
            disconnect(SnoreCorePrivate::instance(), &SnoreCorePrivate::notify, this, &SnoreSecondaryBackend::slotNotify);
            disconnect(SnoreCorePrivate::instance(), &SnoreCorePrivate::notificationDisplayed, this, &SnoreSecondaryBackend::slotNotificationDisplayed);
        }
    });
}

SnoreSecondaryBackend::~SnoreSecondaryBackend()
{
    snoreDebug(SNORE_DEBUG) << "Deleting" << name();
}

void SnoreSecondaryBackend::slotNotify(Notification)
{

}

void SnoreSecondaryBackend::slotNotificationDisplayed(Notification)
{

}

bool SnoreBackend::canCloseNotification() const
{
    return false;
}

bool SnoreBackend::canUpdateNotification() const
{
    return false;
}

void SnoreBackend::slotRegisterApplication(const Application &application)
{
    Q_UNUSED(application);
}

void SnoreBackend::slotDeregisterApplication(const Application &application)
{
    Q_UNUSED(application);
}

void SnoreBackend::slotNotificationDisplayed(Notification notification)
{
    notification.addActiveIn(this);
    SnoreCorePrivate::instance()->slotNotificationDisplayed(notification);
}

void SnoreBackend::slotNotificationActionInvoked(Notification notification, const Action &action)
{
    notification.data()->setActionInvoked(action);
    SnoreCorePrivate::instance()->slotNotificationActionInvoked(notification);
}

