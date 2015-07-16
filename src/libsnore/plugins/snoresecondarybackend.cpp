/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2015  Patrick von Reth <vonreth@kde.org>

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

#include "snoresecondarybackend.h"

#include "../snore_p.h"

using namespace Snore;

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
