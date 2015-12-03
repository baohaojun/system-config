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

#include <QGuiApplication>
#include <QSettings>
#include <QTemporaryDir>

using namespace Snore;

#if QT_VERSION >= QT_VERSION_CHECK(5, 4, 0)
Q_LOGGING_CATEGORY(SNORE, "libsnorenotify", QtWarningMsg)
#else
Q_LOGGING_CATEGORY(SNORE, "libsnorenotify")
#endif

SnoreCorePrivate::SnoreCorePrivate():
    m_localSettingsPrefix(qApp->applicationName().isEmpty() ? QStringLiteral("SnoreNotify") : qApp->applicationName())
{
    if (qEnvironmentVariableIsSet("LIBSNORE_SETTINGS_FILE")) {
        m_settings =  new QSettings(QString::fromUtf8(qgetenv("LIBSNORE_SETTINGS_FILE")), QSettings::IniFormat);
    } else {
        m_settings = new QSettings(QStringLiteral("Snorenotify"), QStringLiteral("libsnore"), this);
    }
    qCDebug(SNORE) << "Version:" << Version::version();
    if (!Version::revision().isEmpty()) {
        qCDebug(SNORE) << "Revision:" << Version::revision();
    }

    qCDebug(SNORE) << "Temp dir is" << tempPath();
    qCDebug(SNORE) << "Snore settings are located in" << m_settings->fileName();
    qCDebug(SNORE) << "Snore local settings are located in" << normalizeSettingsKey(QStringLiteral("Test"), LocalSetting);

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
    if (m_pluginNames[SnorePlugin::Backend].contains(backend)) {
        if (backend == q->primaryNotificationBackend()) {
            return true;
        }
        const QHash<QString, PluginContainer *> backends = PluginContainer::pluginCache(SnorePlugin::Backend);
        if (!backends.contains(backend)) {
            qCDebug(SNORE) << "Unknown Backend:" << backend;
            return false;
        }
        qCDebug(SNORE) << "Setting Notification Backend to:" << backend;
        SnoreBackend *b = qobject_cast<SnoreBackend *>(backends.value(backend)->load());
        if (!b->isReady()) {
            qCDebug(SNORE) << "Backend not ready:" << b->errorString();

            emit q->primaryNotificationBackendError(b->errorString());
            return false;
        }
        if (m_notificationBackend) {
            m_notificationBackend->disable();
        }
        m_notificationBackend = b;
        m_notificationBackend->enable();
        q->setSettingsValue(QStringLiteral("PrimaryBackend"), backend, LocalSetting);

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
    qCDebug(SNORE) << q->settingsValue(QStringLiteral("PrimaryBackend"), LocalSetting).toString();
    if (setBackendIfAvailible(q->settingsValue(QStringLiteral("PrimaryBackend"), LocalSetting).toString())) {
        return true;
    }
#ifdef Q_OS_WIN
    if (QSysInfo::windowsVersion() >= QSysInfo::WV_WINDOWS8 && setBackendIfAvailible(QLatin1String("Windows Toast"))) {
        return true;
    }
    if (setBackendIfAvailible(QStringLiteral("Growl"))) {
        return true;
    }
    if (setBackendIfAvailible(QStringLiteral("Snarl"))) {
        return true;
    }
#elif defined(Q_OS_LINUX)
    if (setBackendIfAvailible(QStringLiteral("Freedesktop"))) {
        return true;
    }
#elif defined(Q_OS_MAC)
    if (setBackendIfAvailible(QStringLiteral("OSX Notification Center"))) {
        return true;
    }
    if (setBackendIfAvailible(QStringLiteral("Growl"))) {
        return true;
    }
#endif
    if (setBackendIfAvailible(QStringLiteral("Snore"))) {
        return true;
    }
    return false;
}

void SnoreCorePrivate::init()
{
    setDefaultSettingsValueIntern(QStringLiteral("Timeout"), 10);
    setDefaultSettingsValueIntern(QStringLiteral("Silent"), false);
    m_defaultApp = Application(QStringLiteral("SnoreNotify"), Icon::defaultIcon());
}

void SnoreCorePrivate::setDefaultSettingsValueIntern(const QString &key, const QVariant &value)
{
    QString nk = normalizeSettingsKey(key + QLatin1String("-SnoreDefault"), LocalSetting);
    if (!m_settings->contains(nk)) {
        m_settings->setValue(nk, value);
    }
}

void SnoreCorePrivate::syncSettings()
{
    Q_Q(SnoreCore);
    QString newBackend = q->settingsValue(QStringLiteral("PrimaryBackend"), LocalSetting).toString();
    if (!newBackend.isEmpty()) {
        QString oldBackend;
        if (m_notificationBackend) {
            oldBackend = m_notificationBackend->name();
            m_notificationBackend->disable();
            m_notificationBackend = nullptr;
        }
        if (!setBackendIfAvailible(newBackend)) {
            qCWarning(SNORE) << "Failed to set new backend" << q->settingsValue(QStringLiteral("PrimaryBackend"), LocalSetting).toString() << "restoring" << oldBackend;
            setBackendIfAvailible(oldBackend);
        }
    }

    auto types = SnorePlugin::types();
    types.removeOne(SnorePlugin::Backend);
    foreach(auto type, types) {
        foreach(auto & pluginName, m_pluginNames[type]) {
            auto key = qMakePair(type, pluginName);
            SnorePlugin *plugin = m_plugins.value(key);
            bool enable = m_plugins[key]->settingsValue(QStringLiteral("Enabled"), LocalSetting).toBool();
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
        qCWarning(SNORE) << n << "was already closed";
    }
    if (!m_notificationQue.isEmpty() && m_activeNotifications.size() < maxNumberOfActiveNotifications()) {
        qCDebug(SNORE) << "Broadcast from queue" << m_notificationQue.size();
        q->broadcastNotification(m_notificationQue.takeFirst());
    }
}

void SnoreCorePrivate::slotAboutToQuit()
{
    for (PluginContainer *p : PluginContainer::pluginCache(SnorePlugin::All)) {
        if (p->isLoaded()) {
            qCDebug(SNORE) << "deinitialize" << p->name();
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
        qCDebug(SNORE) << notification;
        q->requestCloseNotification(notification, Notification::TimedOut);
    });
    timer->start();
}

