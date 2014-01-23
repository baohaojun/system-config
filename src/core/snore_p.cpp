/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2014  Patrick von Reth <vonreth@kde.org>


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


#include "snore_p.h"
#include "snore.h"
#include "plugins/plugins.h"
#include "plugins/snorebackend.h"
#include "plugins/snorefrontend.h"
#include "notification/notification_p.h"

using namespace Snore;


QString const SnoreCorePrivate::snoreTMP()
{
    static QString tmp;
    if(tmp.isNull())
    {
        tmp = QString("%1/libsnore/").arg(QDir::tempPath());
        QDir(tmp).mkpath(".");
    }
    return tmp;
}

SnoreCorePrivate::SnoreCorePrivate(QSystemTrayIcon *trayIcon):
    m_trayIcon(trayIcon),
    m_defaultApp("SnoreNotify",Icon(":/root/snore.png"))
{
    m_defaultApp.addAlert(Alert("Default",Icon(":/root/snore.png")));
}

SnoreCorePrivate::~SnoreCorePrivate()
{

}

const Application SnoreCorePrivate::defaultApplication() const
{
    return m_defaultApp;
}

void SnoreCorePrivate::notificationActionInvoked(Notification notification) const
{
    Q_Q(const SnoreCore);
    emit const_cast<SnoreCore*>(q)->actionInvoked(notification);
    if ( notification.data()->source() )
    {
        notification.data()->source()->actionInvoked ( notification );
    }
}

bool SnoreCorePrivate::setBackendIfAvailible(const QString &backend)
{
    Q_Q(SnoreCore);
    if( m_plugins.contains(SnorePlugin::BACKEND, backend))
    {
        return q->setPrimaryNotificationBackend(backend);
    }
    return false;
}

void SnoreCorePrivate::slotNotificationClosed(Notification n)
{
    Q_Q(SnoreCore);
    emit q->notificationClosed(n);
    if(n.data()->source())
    {
        n.data()->source()->notificationClosed(n);
    }
}

