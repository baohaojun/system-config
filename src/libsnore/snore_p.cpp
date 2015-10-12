/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2014-2015  Hannah von Reth <vonreth@kde.org>

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
#include <QSettings>
#include <QTemporaryDir>

using namespace Snore;

SnoreCorePrivate::SnoreCorePrivate():
    m_localSettingsPrefix(qApp->applicationName().isEmpty() ? QLatin1String("SnoreNotify") : qApp->applicationName())
{
    if (!qgetenv("LIBSNORE_SETTINGS_FILE").isNull()) {
        m_settings =  new QSettings(QString::fromUtf8(qgetenv("LIBSNORE_SETTINGS_FILE")), QSettings::IniFormat);
    } else {
        m_settings = new QSettings(QLatin1String("Snorenotify"), QLatin1String("libsnore"), this);
    }
    snoreDebug(SNORE_INFO) << "Version:" << Version::version();
    if (!Version::revision().isEmpty()) {
        snoreDebug(SNORE_INFO) << "Revision:" << Version::revision();
    }

    snoreDebug(SNORE_DEBUG) << "Temp dir is" << tempPath();
    snoreDebug(SNORE_DEBUG) << "Snore settings are located in" << m_settings->fileName();
    snoreDebug(SNORE_DEBUG) << "Snore local settings are located in" << normalizeSettingsKey(QLatin1String("Test"), LOCAL_SETTING);

    connect(qApp, SIGNAL(aboutToQuit()), this, SLOT(slotAboutToQuit()));
}

SnoreCorePrivate::~SnoreCorePrivate()
{

}

Application SnoreCorePrivate::defaultApplication()
{
    if (!SnoreCore::instance().aplications().contains(m_defaultApp.key())) {
        SnoreCore::instance().registerApplication(m_defaultApp);
    }
    return m_defaultApp;
}

void SnoreCorePrivate::slotNotificationActionInvoked(Notification notification)
{
    Q_Q(SnoreCore);
    emit q->actionInvoked(notification);
}

void SnoreCorePrivate::slotNotificationDisplayed(Notification notification)
{
    emit notificationDisplayed(notification);
    startNotificationTimeoutTimer(notification);
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
        if (!b->isReady()) {
            snoreDebug(SNORE_DEBUG) << "Backend not ready:" << b->errorString();

            emit q->primaryNotificationBackendError(b->errorString());
            return false;
        }
        if (m_notificationBackend) {
            m_notificationBackend->disable();
        }
        m_notificationBackend = b;
        m_notificationBackend->enable();
        q->setSettingsValue(QLatin1String("PrimaryBackend"), backend, LOCAL_SETTING);

        connect(b, &SnoreBackend::error, [this, b](const QString &) {
            slotInitPrimaryNotificationBackend();
        });
        emit q->primaryNotificationBackendChanged(b->name());
        return true;
    }
    return false;
}

bool SnoreCorePrivate::slotInitPrimaryNotificationBackend()
{
    Q_Q(SnoreCore);
    snoreDebug(SNORE_DEBUG) << q->settingsValue(QLatin1String("PrimaryBackend"), LOCAL_SETTING).toString();
    if (setBackendIfAvailible(q->settingsValue(QLatin1String("PrimaryBackend"), LOCAL_SETTING).toString())) {
        return true;
    }
#ifdef Q_OS_WIN
    if (QSysInfo::windowsVersion() >= QSysInfo::WV_WINDOWS8 && setBackendIfAvailible(QLatin1String("Windows Toast"))) {
        return true;
    }
    if (setBackendIfAvailible(QLatin1String("Growl"))) {
        return true;
    }
    if (setBackendIfAvailible(QLatin1String("Snarl"))) {
        return true;
    }
#elif defined(Q_OS_LINUX)
    if (setBackendIfAvailible(QLatin1String("Freedesktop"))) {
        return true;
    }
#elif defined(Q_OS_MAC)
    if (setBackendIfAvailible(QLatin1String("OSX Notification Center"))) {
        return true;
    }
    if (setBackendIfAvailible(QLatin1String("Growl"))) {
        return true;
    }
#endif
    if (setBackendIfAvailible(QLatin1String("Snore"))) {
        return true;
    }
    return false;
}

void SnoreCorePrivate::init()
{
    setDefaultSettingsValueIntern(QLatin1String("Timeout"), 10);
    setDefaultSettingsValueIntern(QLatin1String("Silent"), false);
    m_defaultApp = Application(QLatin1String("SnoreNotify"), Icon::defaultIcon());
}

void SnoreCorePrivate::setDefaultSettingsValueIntern(const QString &key, const QVariant &value)
{
    QString nk = normalizeSettingsKey(key + QLatin1String("-SnoreDefault"), LOCAL_SETTING);
    if (!m_settings->contains(nk)) {
        m_settings->setValue(nk, value);
    }
}

void SnoreCorePrivate::syncSettings()
{
    Q_Q(SnoreCore);
    QString newBackend = q->settingsValue(QLatin1String("PrimaryBackend"), LOCAL_SETTING).toString();
    if (!newBackend.isEmpty()) {
        QString oldBackend;
        if (m_notificationBackend) {
            oldBackend = m_notificationBackend->name();
            m_notificationBackend->disable();
            m_notificationBackend = nullptr;
        }
        if (!setBackendIfAvailible(newBackend)) {
            snoreDebug(SNORE_WARNING) << "Failed to set new backend" << q->settingsValue(QLatin1String("PrimaryBackend"), LOCAL_SETTING).toString() << "restoring" << oldBackend;
            setBackendIfAvailible(oldBackend);
        }
    }

    auto types = SnorePlugin::types();
    types.removeOne(SnorePlugin::BACKEND);
    for (auto type : types) {
        for (auto &pluginName : m_pluginNames[type]) {
            auto key = qMakePair(type, pluginName);
            SnorePlugin *plugin = m_plugins.value(key);
            bool enable = m_plugins[key]->settingsValue(QLatin1String("Enabled"), LOCAL_SETTING).toBool();
            plugin->setEnabled(enable);
        }
    }
}

QSettings &SnoreCorePrivate::settings()
{
    return *m_settings;
}

void SnoreCorePrivate::setLocalSttingsPrefix(const QString &prefix)
{
    m_localSettingsPrefix = prefix;
    init();
    syncSettings();
}

QString SnoreCorePrivate::tempPath()
{
#if 1
    static QTemporaryDir dir;
    return dir.path();
#else
    return QDir::temp().path() + QLatin1String("/libsnore/");
#endif
}

// TODO: this is somehow horrible code
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
    if (!n.removeActiveIn(q)) {
        snoreDebug(SNORE_WARNING) << n << "was already closed";
    }
    if (!m_notificationQue.isEmpty() && m_activeNotifications.size() < maxNumberOfActiveNotifications()) {
        snoreDebug(SNORE_DEBUG) << "Broadcast from queue" << m_notificationQue.size();
        q->broadcastNotification(m_notificationQue.takeFirst());
    }
}

void SnoreCorePrivate::slotAboutToQuit()
{
    for (PluginContainer *p : PluginContainer::pluginCache(SnorePlugin::ALL)) {
        if (p->isLoaded()) {
            snoreDebug(SNORE_DEBUG) << "deinitialize" << p->name();
            p->load()->disable();
        }
    }
}

void SnoreCorePrivate::startNotificationTimeoutTimer(Notification notification)
{
    Q_Q(SnoreCore);
    if (notification.isSticky()) {
        return;
    }

    notification.data()->stopTimeoutTimer();
    QTimer *timer = new QTimer();
    notification.data()->m_timeoutTimer = timer;
    timer->setSingleShot(true);

    if (notification.isUpdate()) {
        notification.old().data()->stopTimeoutTimer();
    }
    timer->setInterval(notification.timeout() * 1000);
    connect(timer, &QTimer::timeout, [q, notification]() {
        snoreDebug(SNORE_DEBUG) << notification;
        q->requestCloseNotification(notification, Notification::TIMED_OUT);
    });
    timer->start();
}

