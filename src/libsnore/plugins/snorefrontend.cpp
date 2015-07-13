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

#include "snorefrontend.h"
#include "../snore.h"

using namespace Snore;

SnoreFrontend::SnoreFrontend()
{
    connect(this, &SnoreFrontend::enabledChanged, [this](bool enabled) {
        if (enabled) {
            connect(&SnoreCore::instance(), &SnoreCore::notificationClosed, this, &SnoreFrontend::slotNotificationClosed, Qt::QueuedConnection);
            connect(&SnoreCore::instance(), &SnoreCore::actionInvoked, this, &SnoreFrontend::slotActionInvoked, Qt::QueuedConnection);
        } else {
            disconnect(&SnoreCore::instance(), &SnoreCore::notificationClosed, this, &SnoreFrontend::slotNotificationClosed);
            disconnect(&SnoreCore::instance(), &SnoreCore::actionInvoked, this, &SnoreFrontend::slotActionInvoked);
        }
    });
}

SnoreFrontend::~SnoreFrontend()
{
    snoreDebug(SNORE_DEBUG) << "Deleting" << name();
}

void SnoreFrontend::slotActionInvoked(Notification)
{

}

void SnoreFrontend::slotNotificationClosed(Notification)
{

}
