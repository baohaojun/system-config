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

#include "snore.h"
#include "snore_p.h"
#include "lambdahint.h"
#include "notification/notification.h"
#include "notification/notification_p.h"
#include "plugins/plugincontainer.h"
#include "plugins/plugins.h"
#include "plugins/snorebackend.h"
#include "plugins/snorefrontend.h"

#include "version.h"

#include <QGuiApplication>
#include <QSettings>
#include <QThread>

using namespace Snore;

SnoreCore::SnoreCore(QObject* parent):
    QObject(parent)
{
    if (QThread::currentThread() != parent->thread()) {
        moveToThread(parent->thread());
    }
    d_ptr = new SnoreCorePrivate();
    Q_D(SnoreCore);
    d->q_ptr = this;
}

SnoreCore& SnoreCore::instance()
{
    static SnoreCore* instance = nullptr;
    if (!instance) {
        qRegisterMetaType<Application>();
        qRegisterMetaType<LambdaHint>();
        qRegisterMetaType<Notification>();
        qRegisterMetaType<SnorePlugin::PluginTypes>();
        qRegisterMetaTypeStreamOperators<SnorePlugin::PluginTypes>();
        instance = new SnoreCore(qApp);
        SnoreCorePrivate::instance()->init();
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
        qCDebug(SNORE) << "Delayed Plugin loading." << QThread::currentThread() << thread();
        QMetaObject::invokeMethod(this, "loadPlugins", Qt::BlockingQueuedConnection, Q_ARG(Snore::SnorePlugin::PluginTypes, types));
        return;
    }
    Q_D(SnoreCore);
    setSettingsValue(QStringLiteral("PluginTypes"), QVariant::fromValue(types), LocalSetting);
    qCDebug(SNORE) << "Loading plugin types:" << types;
    foreach (const SnorePlugin::PluginTypes type, SnorePlugin::types()) {
        if (type != SnorePlugin::All && types & type) {
            foreach (PluginContainer * info, PluginContainer::pluginCache(type).values()) {
                SnorePlugin* plugin = info->load();
                if (!plugin) {
                    continue;
                }

                switch (info->type()) {
                case SnorePlugin::Backend:
                    break;
                case SnorePlugin::SecondaryBackend:
                case SnorePlugin::Frontend:
                case SnorePlugin::Plugin:
                case SnorePlugin::Settings:
                    plugin->setEnabled(plugin->settingsValue(QStringLiteral("Enabled"), LocalSetting).toBool());
                    break;
                default:
                    qCWarning(SNORE) << "Plugin Cache corrupted\n" << info->file() << info->type();
                    continue;
                }

                qCDebug(SNORE) << info->name() << "is a" << info->type();
                d->m_pluginNames[info->type()].append(info->name());
                auto key = qMakePair(type, info->name());
                Q_ASSERT_X(!d->m_plugins.contains(key), Q_FUNC_INFO, "Multiple plugins of the same type with the same name.");
                d->m_plugins.insert(key, plugin);
            }
            if (d->m_pluginNames.contains(type)) {
                qSort(d->m_pluginNames[type]);
            }
        }
    }
    d->slotInitPrimaryNotificationBackend();
    qCDebug(SNORE) << "Loaded Plugins:" << d->m_pluginNames;
}

void SnoreCore::broadcastNotification(Notification notification)
{
    Q_D(SnoreCore);
    if (d->m_activeNotifications.size() > d->maxNumberOfActiveNotifications()) {
        qCDebug(SNORE) << "queue size:" << d->m_notificationQue.size() << "active size:" << d->m_activeNotifications.size();
        d->m_notificationQue.append(notification);
        return;
    }
    Q_ASSERT_X(!notification.data()->isBroadcasted(), Q_FUNC_INFO, "Notification was already broadcasted.");
    qCDebug(SNORE) << "Broadcasting" << notification << "timeout:" << notification.timeout();
    if (d->m_notificationBackend != nullptr) {
        if (notification.isUpdate() && !d->m_notificationBackend->canUpdateNotification()) {
            requestCloseNotification(notification.old(), Notification::Replaced);
        }
    }
    notification.data()->setBroadcasted();
    notification.addActiveIn(this);
    if (!d->m_notificationBackend) {
        d->startNotificationTimeoutTimer(notification);
    }
    emit d->notify(notification);
}

void SnoreCore::registerApplication(const Application& application)
{
    Q_D(SnoreCore);
    Q_ASSERT_X(!d->m_applications.contains(application.key()), Q_FUNC_INFO,
               "Applications mus be registered only once.");
    qCDebug(SNORE) << "Registering Application:" << application;
    d->m_applications.insert(application.key(), application);
    emit d->applicationRegistered(application);
}

void SnoreCore::deregisterApplication(const Application& application)
{
    Q_D(SnoreCore);
    emit d->applicationDeregistered(application);
    d->m_applications.take(application.key());
}

const QHash<QString, Application>& SnoreCore::aplications() const
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

bool SnoreCore::setPrimaryNotificationBackend(const QString& backend)
{
    Q_D(SnoreCore);
    return d->setBackendIfAvailible(backend);
}

void SnoreCore::requestCloseNotification(Notification n, Notification::CloseReasons r)
{
    Q_D(SnoreCore);
    bool wasQued  = d->m_notificationQue.removeOne(n);
    if (wasQued) {
        qCDebug(SNORE) << n << " was qued.";
    }
    if (!wasQued && d->m_notificationBackend) {
        d->m_notificationBackend->requestCloseNotification(n, r);
    } else {
        if (n.isValid()) {
            n.data()->setCloseReason(r);
            emit notificationClosed(n);
        }
    }
}

void SnoreCore::setDefaultApplication(const Application app)
{
    Q_D(SnoreCore);
    d->m_defaultApp = app;
}

QVariant SnoreCore::settingsValue(const QString& key, SettingsType type) const
{
    Q_D(const SnoreCore);
    QString nk = d->normalizeSettingsKey(key, type);
    if (type == LocalSetting && !d->m_settings->contains(nk)) {
        nk = d->normalizeSettingsKey(key + QStringLiteral("-SnoreDefault"), type);
    }
    return d->m_settings->value(nk);
}

void SnoreCore::setSettingsValue(const QString& key, const QVariant& value, SettingsType type)
{
    Q_D(SnoreCore);
    d->m_settings->setValue(d->normalizeSettingsKey(key, type), value);
}

void SnoreCore::setDefaultSettingsValue(const QString& key, const QVariant& value, SettingsType type)
{
    Q_D(SnoreCore);
    QString nk = d->normalizeSettingsKey(key, type);
    if (!d->m_settings->contains(nk)) {
        qCDebug(SNORE) <<  "Set default value" << nk << value;
        d->m_settings->setValue(nk, value);
    }
}

Notification SnoreCore::getActiveNotificationByID(uint id) const
{
    Q_D(const SnoreCore);
    return d->m_activeNotifications.value(id);
}

void SnoreCore::displayExampleNotification()
{
    Application app = SnoreCorePrivate::instance()->defaultApplication();
    QString text = QLatin1String("<i>") + tr("This is %1").arg(app.name()) + QLatin1String("</i><br>"
                   "<b>") + tr("Everything is awesome!") + QLatin1String("</b><br>");
    if (!app.constHints().value("use-markup").toBool()) {
        text = Utils::normalizeMarkup(text, Utils::NoMarkup);
    }
    Notification noti(app, app.defaultAlert(), tr("Hello There!"), text, app.icon());
    noti.addAction(Action(1, tr("Awesome Action!")));
    broadcastNotification(noti);
}
