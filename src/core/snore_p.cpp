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
    if (m_pluginNames[SnorePlugin::BACKEND].contains(backend)) {
        if (backend == q->primaryNotificationBackend()) {
            return true;
        }
        const QHash<QString, PluginContainer *> backends = PluginContainer::pluginCache(SnorePlugin::BACKEND);
        if (!backends.contains(backend)) {
            snoreDebug(SNORE_DEBUG) << "Unknown Backend:" << backend;
            return false;
        }
        snoreDebug(SNORE_DEBUG) << "Setting Notification Backend to:" << backend;
        SnoreBackend *b = qobject_cast<SnoreBackend *>(backends.value(backend)->load());
        if (!b->isInitialized()) {
            if (!b->initialize()) {
                snoreDebug(SNORE_DEBUG) << "Failed to initialize" << b->name();
                return false;
            }
        }
        if (m_notificationBackend) {
            m_notificationBackend->deinitialize();
        }

        m_notificationBackend = b;
        q->setValue("PrimaryBackend", Setting(backend,true));
        return true;
    }
    return false;
}

bool SnoreCorePrivate::initPrimaryNotificationBackend()
{
    Q_Q(SnoreCore);
    snoreDebug(SNORE_DEBUG)<<q->value("PrimaryBackend").value<Setting>().value().toString();
    if (setBackendIfAvailible(q->value("PrimaryBackend").value<Setting>().value().toString())) {
        return true;
    }
#ifdef Q_OS_WIN
    if (QSysInfo::windowsVersion() >= QSysInfo::WV_WINDOWS8 && setBackendIfAvailible("Windows 8")) {
        return true;
    }
    if (setBackendIfAvailible("Growl")) {
        return true;
    }
    if (setBackendIfAvailible("Snarl")) {
        return true;
    }
#elif defined(Q_OS_LINUX)
    if (setBackendIfAvailible("FreedesktopNotification")) {
        return true;
    }
#elif defined(Q_OS_MAC)
    if (setBackendIfAvailible("OSX Notification Center")) {
        return true;
    }
    if (setBackendIfAvailible("Growl")) {
        return true;
    }
#endif
    if (setBackendIfAvailible("Snore")) {
        return true;
    }
    return false;
}

void SnoreCorePrivate::syncSettings()
{
    setBackendIfAvailible(m_settings->value("PrimaryBackend").value<Setting>().value().toString());
    for (auto pluginName : m_pluginNames[SnorePlugin::SECONDARY_BACKEND]) {
        SnorePlugin *plugin = m_plugins.value(pluginName);
        bool enable = m_plugins[pluginName]->value("Enabled").toBool();
        if (!plugin->isInitialized() && enable) {
            plugin->initialize();
        } else if (plugin->isInitialized() && !enable) {
            plugin->deinitialize();
        }
    }
}

void SnoreCorePrivate::registerMetaTypes()
{
    qRegisterMetaType<Notification>();
    qRegisterMetaType<Application>();
    qRegisterMetaType<Setting>();
    qRegisterMetaTypeStreamOperators<Setting>("Snore::Settings");
}

QString SnoreCorePrivate::tempPath()
{
    static QTemporaryDir dir;
    return dir.path();
}

SnoreCorePrivate *SnoreCorePrivate::instance()
{
    return SnoreCore::instance().d_ptr;
}

bool SnoreCorePrivate::primaryBackendCanUpdateNotification() const
{
    return m_notificationBackend->canUpdateNotification();
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
    for(PluginContainer * p : PluginContainer::pluginCache(SnorePlugin::ALL)) {
        if (p->isLoaded()) {
            snoreDebug(SNORE_DEBUG) << "deinitialize" << p->name();
            p->load()->deinitialize();
        }
    }
}

