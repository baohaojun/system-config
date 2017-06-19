/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2015  Hannah von Reth <vonreth@kde.org>

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

#include "pushover_frontend.h"

#include "libsnore/snore.h"
#include "libsnore/version.h"
#include "libsnore/notification/notification_p.h"
#include "libsnore/hint.h"

using namespace Snore;

PushoverFrontend::PushoverFrontend():
    m_client(new PushoverClient(this))
{
    hints().setValue("client", QVariant::fromValue(QPointer<PushoverClient>(m_client)));
    connect(this, &PushoverFrontend::enabledChanged, [this](bool enabled) {
        if (enabled) {
            m_client->connectToService();
        } else {
            m_client->disconnectService();
        }
    });
}

void PushoverFrontend::setDefaultSettings()
{
    setDefaultSettingsValue(QStringLiteral("Secret"), QString(), LocalSetting);
    setDefaultSettingsValue(QStringLiteral("DeviceID"), QString(), LocalSetting);
    SnoreFrontend::setDefaultSettings();
}

void PushoverFrontend::slotActionInvoked(Notification notification)
{
    if (notification.priority() == Notification::Emergency) {
        qCWarning(SNORE) << "emergeency notification" << notification;
        m_client->acknowledgeNotification(notification);
    }
}

