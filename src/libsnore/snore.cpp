/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2015  Patrick von Reth <vonreth@kde.org>

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

#include "snore.h"
#include "snore_p.h"
#include "notification/notification.h"
#include "notification/notification_p.h"
#include "plugins/plugincontainer.h"
#include "plugins/plugins.h"
#include "plugins/snorebackend.h"
#include "plugins/snorefrontend.h"

#include "version.h"

#include <iostream>

#include <QSettings>
#include <QThread>

using namespace Snore;

SnoreCore::SnoreCore(QObject *parent):
    QObject(parent)
{
    if (QThread::currentThread() != parent->thread()) {
        moveToThread(parent->thread());
    }
    d_ptr = new SnoreCorePrivate();
    Q_D(SnoreCore);
    d->q_ptr = this;
    d->init();
}

SnoreCore &SnoreCore::instance()
{
    static SnoreCore *instance = nullptr;
    if (!instance) {
        SnoreCorePrivate::loadTranslator();
        SnoreCorePrivate::registerMetaTypes();
        instance = new SnoreCore(qApp);
    }
    return *instance;
}

SnoreCore::~SnoreCore()
{
    Q_D(SnoreCore);
    d->deleteLater();
}

void SnoreCore::loadPlugins(SnorePlugin::PluginTypes types)
{
    if (QThread::currentThread() != thread()) {
        snoreDebug(SNORE_DEBUG) << "Delayed Plugin loading." << QThread::currentThread() << thread();
        QMetaObject::invokeMethod(this, "loadPlugins", Qt::BlockingQueuedConnection, Q_ARG(Snore::SnorePlugin::PluginTypes, types));
        return;
    }
    Q_D(SnoreCore);
    setValue("PluginTypes", QVariant::fromValue(types), LOCAL_SETTING);
    for (SnorePlugin::PluginTypes type : SnorePlugin::types()) {
        if (type != SnorePlugin::ALL && types & type) {
            for (PluginContainer *info : PluginContainer::pluginCache(type).values()) {
                SnorePlugin *plugin = info->load();
                if (!plugin) {
                    continue;
                }
                switch (info->type()) {
                case SnorePlugin::BACKEND:
                    break;
                case SnorePlugin::SECONDARY_BACKEND:
                case SnorePlugin::FRONTEND:
                case SnorePlugin::PLUGIN: {
                    if (plugin->value("Enabled", LOCAL_SETTING).toBool()) {
                        if (!plugin->initialize()) {
                            snoreDebug(SNORE_WARNING) << "Failed to initialize" << plugin->name();
                            plugin->deinitialize();
                            //info->unload();
                            break;
                        }
                    }
                }
                break;
                default:
                    snoreDebug(SNORE_WARNING) << "Plugin Cache corrupted\n" << info->file() << info->type();
                    continue;
                }
                snoreDebug(SNORE_DEBUG) << info->name() << "is a" << info->type();
                d->m_pluginNames[info->type()].append(info->name());
                d->m_plugins[info->name()] = plugin;
            }
            if (d->m_pluginNames.contains(type)) {
                qSort(d->m_pluginNames[type]);
            }
        }
    }
    d->initPrimaryNotificationBackend();
    snoreDebug(SNORE_INFO) << "Loaded Plugins:" << d->m_pluginNames;
}

void SnoreCore::broadcastNotification(Notification notification)
{
    Q_D(SnoreCore);
    snoreDebug(SNORE_DEBUG) << "Broadcasting" << notification << "timeout:" << notification.timeout();
    if (d->m_notificationBackend != nullptr) {
        if (notification.isUpdate() && !d->m_notificationBackend->canUpdateNotification()) {
            requestCloseNotification(notification.old(), Notification::REPLACED);
        }
        notification.addActiveIn(d->m_notificationBackend);
    }
    emit d->notify(notification);
}

void SnoreCore::registerApplication(const Application &application)
{
    Q_D(SnoreCore);
    if (!d->m_applications.contains(application.name())) {
        snoreDebug(SNORE_DEBUG) << "Registering Application:" << application;
        d->m_applications.insert(application.name(), application);
        emit d->applicationRegistered(application);
    }
}

void SnoreCore::deregisterApplication(const Application &application)
{
    Q_D(SnoreCore);
    emit d->applicationDeregistered(application);
    d->m_applications.take(application.name());
}

const QHash<QString, Application> &SnoreCore::aplications() const
{
    Q_D(const SnoreCore);
    return d->m_applications;
}

const QStringList SnoreCore::pluginNames(SnorePlugin::PluginTypes type) const
{
    Q_D(const SnoreCore);
    QStringList out;
    for (auto t : SnorePlugin::types()) {
        if (t & type) {
            out.append(d->m_pluginNames.value(t));
        }
    }
    return out;
}

const QString SnoreCore::primaryNotificationBackend() const
{
    Q_D(const SnoreCore);
    if (d->m_notificationBackend.isNull()) {
        return QString::null;
    }
    return d->m_notificationBackend->name();
}

bool SnoreCore::setPrimaryNotificationBackend(const QString &backend)
{
    Q_D(SnoreCore);
    return d->setBackendIfAvailible(backend);
}

void SnoreCore::requestCloseNotification(Notification n, Notification::CloseReasons r)
{
    Q_D(SnoreCore);
    d->m_notificationBackend->requestCloseNotification(n, r);
}

bool SnoreCore::primaryBackendSupportsRichtext()
{
    Q_D(SnoreCore);
    return d->m_notificationBackend->supportsRichtext();
}

void SnoreCore::setDefaultApplication(Application app)
{
    Q_D(SnoreCore);
    d->m_defaultApp = app;
}

QList<PluginSettingsWidget *> SnoreCore::settingWidgets(SnorePlugin::PluginTypes type)
{
    Q_D(SnoreCore);
    QList<PluginSettingsWidget *> list;
    for (auto name : d->m_pluginNames[type]) {
//TODO: mem leak?
        SnorePlugin *p = d->m_plugins[name];
        PluginSettingsWidget *widget = p->settingsWidget();
        if (widget) {
            list.append(widget);
        }
    }
    qSort(list.begin(), list.end(), [](PluginSettingsWidget * a, PluginSettingsWidget * b) {
        return a->name() < b->name();
    });
    return list;
}

QVariant SnoreCore::value(const QString &key, SettingsType type) const
{
    Q_D(const SnoreCore);
    return d->m_settings->value(d->normalizeKey(key, type));
}

void SnoreCore::setValue(const QString &key, const QVariant &value, SettingsType type)
{
    Q_D(SnoreCore);
    d->m_settings->setValue(d->normalizeKey(key, type), value);
}

void SnoreCore::setDefaultValue(const QString &key, const QVariant &value, SettingsType type)
{
    Q_D(SnoreCore);
    QString nk = d->normalizeKey(key, type);
    if (!d->m_settings->contains(nk)) {
        d->m_settings->setValue(nk, value);
    }
}


Notification SnoreCore::getActiveNotificationByID(uint id) const
{
    Q_D(const SnoreCore);
    return d->m_activeNotifications.value(id);
}
