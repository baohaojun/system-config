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

#include <QApplication>
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
}

SnoreCore &SnoreCore::instance()
{
    static SnoreCore *instance = nullptr;
    if (!instance) {
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
        snoreDebug(SNORE_DEBUG) << "Delayed Plugin loading." << QThread::currentThread() << thread();
        QMetaObject::invokeMethod(this, "loadPlugins", Qt::BlockingQueuedConnection, Q_ARG(Snore::SnorePlugin::PluginTypes, types));
        return;
    }
    Q_D(SnoreCore);
    setSettingsValue(QLatin1String("PluginTypes"), QVariant::fromValue(types), LOCAL_SETTING);
    snoreDebug(SNORE_DEBUG) << "Loading plugin types:" << types;
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
                case SnorePlugin::PLUGIN:
                    plugin->setEnabled(plugin->settingsValue(QLatin1String("Enabled"), LOCAL_SETTING).toBool());
                    break;
                default:
                    snoreDebug(SNORE_WARNING) << "Plugin Cache corrupted\n" << info->file() << info->type();
                    continue;
                }

                snoreDebug(SNORE_DEBUG) << info->name() << "is a" << info->type();
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
    snoreDebug(SNORE_INFO) << "Loaded Plugins:" << d->m_pluginNames;
}

void SnoreCore::broadcastNotification(Notification notification)
{
    Q_D(SnoreCore);
    if(d->m_activeNotifications.size() > d->maxNumberOfActiveNotifications())
    {
        snoreDebug(SNORE_DEBUG) << "queue size:" << d->m_notificationQue.size() << "active size:" << d->m_activeNotifications.size();
        d->m_notificationQue.append(notification);
        return;
    }
    Q_ASSERT_X(!notification.data()->isBroadcasted(), Q_FUNC_INFO, "Notification was already broadcasted.");
    snoreDebug(SNORE_DEBUG) << "Broadcasting" << notification << "timeout:" << notification.timeout();
    if (d->m_notificationBackend != nullptr) {
        if (notification.isUpdate() && !d->m_notificationBackend->canUpdateNotification()) {
            requestCloseNotification(notification.old(), Notification::REPLACED);
        }
    }
    notification.data()->setBroadcasted();
    notification.addActiveIn(this);
    if(!d->m_notificationBackend)
    {
        d->startNotificationTimeoutTimer(notification);
    }
    emit d->notify(notification);
}

void SnoreCore::registerApplication(const Application &application)
{
    Q_D(SnoreCore);
    Q_ASSERT_X(!d->m_applications.contains(application.key()), Q_FUNC_INFO,
               "Applications mus be registered only once.");
    snoreDebug(SNORE_DEBUG) << "Registering Application:" << application;
    d->m_applications.insert(application.key(), application);
    emit d->applicationRegistered(application);
}

void SnoreCore::deregisterApplication(const Application &application)
{
    Q_D(SnoreCore);
    emit d->applicationDeregistered(application);
    d->m_applications.take(application.key());
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

    if (d->m_notificationBackend) {
        d->m_notificationBackend->requestCloseNotification(n, r);
    } else {
        if (n.isValid()) {
            n.data()->setCloseReason(r);
            emit notificationClosed(n);
        }
    }
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
        SnorePlugin *p = d->m_plugins[qMakePair(type, name)];
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

QVariant SnoreCore::settingsValue(const QString &key, SettingsType type) const
{
    Q_D(const SnoreCore);
    QString nk = d->normalizeSettingsKey(key, type);
    if (type == LOCAL_SETTING && !d->m_settings->contains(nk)) {
        nk = d->normalizeSettingsKey(key + QStringLiteral("-SnoreDefault"), type);
    }
    return d->m_settings->value(nk);
}

void SnoreCore::setSettingsValue(const QString &key, const QVariant &value, SettingsType type)
{
    Q_D(SnoreCore);
    d->m_settings->setValue(d->normalizeSettingsKey(key, type), value);
}

void SnoreCore::setDefaultSettingsValue(const QString &key, const QVariant &value, SettingsType type)
{
    Q_D(SnoreCore);
    QString nk = d->normalizeSettingsKey(key, type);
    if (!d->m_settings->contains(nk)) {
        snoreDebug(SNORE_DEBUG) <<  "Set default value" << nk << value;
        d->m_settings->setValue(nk, value);
    }
}

Notification SnoreCore::getActiveNotificationByID(uint id) const
{
    Q_D(const SnoreCore);
    return d->m_activeNotifications.value(id);
}

void SnoreCore::displayExapleNotification()
{
    Application app = SnoreCorePrivate::instance()->defaultApplication();
    QString text = QLatin1String("<i>") + tr("This is ") + app.name() + QLatin1String("</i><br>"
                   "<b>") + tr("Everything is awesome!") + QLatin1String("</b><br>");
    if (!app.constHints().value("use-markup").toBool()) {
        text = Utils::normalizeMarkup(text, Utils::NO_MARKUP);
    }
    Notification noti(app, app.defaultAlert(), tr("Hello There!"), text, app.icon());
    noti.addAction(Action(1, tr("Awesome Action!")));
    broadcastNotification(noti);
}
