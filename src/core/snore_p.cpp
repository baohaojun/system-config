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
#include "plugins/plugincontainer.h"
#include "notification/notification_p.h"
#include "version.h"

#include <QApplication>
#include <QTemporaryDir>

using namespace Snore;

SnoreCorePrivate::SnoreCorePrivate():
    m_defaultApp("SnoreNotify", Icon(":/root/snore.png")),
    m_settings(new QSettings("Snorenotify", "libsnore", this))
{
    snoreDebug(SNORE_INFO) << "Version:" << Version::version();
    if (!Version::revision().isEmpty()) {
        snoreDebug(SNORE_INFO) << "Revision:" << Version::revision();
    }

    snoreDebug(SNORE_DEBUG) << "Temp dir is" << tempPath();
    snoreDebug(SNORE_DEBUG) << "Snore settings are located in" << m_settings->fileName();

    m_defaultApp.addAlert(Alert("Default", Icon(":/root/snore.png")));
    connect(qApp, SIGNAL(aboutToQuit()), this, SLOT(slotAboutToQuit()));
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
    emit const_cast<SnoreCore *>(q)->actionInvoked(notification);
    if (notification.data()->source()) {
        notification.data()->source()->actionInvoked(notification);
    }
}

bool SnoreCorePrivate::setBackendIfAvailible(const QString &backend)
{
    Q_Q(SnoreCore);
    if (m_plugins[SnorePlugin::BACKEND].contains(backend)) {
        return q->setPrimaryNotificationBackend(backend);
    }
    return false;
}

void SnoreCorePrivate::registerMetaTypes()
{
    qRegisterMetaType<Notification>();
    qRegisterMetaType<Application>();
}

QString SnoreCorePrivate::tempPath()
{
    static QTemporaryDir dir;
    return dir.path();
}

bool SnoreCorePrivate::primaryBackendCanUpdateNotification() const
{
    return m_notificationBackend->canUpdateNotification();
}

void SnoreCorePrivate::addSettingsDescription(const QString &key, const QString &help) const
{
    m_help[key] = help;
}

void SnoreCorePrivate::slotNotificationClosed(Notification n)
{
    Q_Q(SnoreCore);
    emit q->notificationClosed(n);
    if (n.data()->source()) {
        n.data()->source()->notificationClosed(n);
    }
}

void SnoreCorePrivate::slotAboutToQuit()
{
    foreach(PluginContainer * p, PluginContainer::pluginCache(SnorePlugin::ALL)) {
        if (p->isLoaded()) {
            snoreDebug(SNORE_DEBUG) << "deinitialize" << p->name();
            p->load()->deinitialize();
        }
    }
}

