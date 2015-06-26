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
#include <QTranslator>

using namespace Snore;

SnoreCorePrivate::SnoreCorePrivate():
    m_settings(new QSettings("Snorenotify", "libsnore", this))
{
    snoreDebug(SNORE_INFO) << "Version:" << Version::version();
    if (!Version::revision().isEmpty()) {
        snoreDebug(SNORE_INFO) << "Revision:" << Version::revision();
    }

    snoreDebug(SNORE_DEBUG) << "Temp dir is" << tempPath();
    snoreDebug(SNORE_DEBUG) << "Snore settings are located in" << m_settings->fileName();
    snoreDebug(SNORE_DEBUG) << "Snore local settings are located in" << normalizeKey("Test", LOCAL_SETTING);

    connect(qApp, SIGNAL(aboutToQuit()), this, SLOT(slotAboutToQuit()));

    m_defaultApp = Application("SnoreNotify", Icon(":/root/snore.png"));
}

SnoreCorePrivate::~SnoreCorePrivate()
{

}

Application SnoreCorePrivate::defaultApplication()
{
    if (!SnoreCore::instance().aplications().contains(m_defaultApp.name())) {
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
        q->setValue("PrimaryBackend", backend, LOCAL_SETTING);
        return true;
    }
    return false;
}

bool SnoreCorePrivate::initPrimaryNotificationBackend()
{
    Q_Q(SnoreCore);
    snoreDebug(SNORE_DEBUG) << q->value("PrimaryBackend", LOCAL_SETTING).toString();
    if (setBackendIfAvailible(q->value("PrimaryBackend", LOCAL_SETTING).toString())) {
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

void SnoreCorePrivate::init()
{
    Q_Q(SnoreCore);
    setDefaultValueIntern("Timeout", 10);
    setDefaultValueIntern("Silent", false);
}

void SnoreCorePrivate::setDefaultValueIntern(const QString &key, const QVariant &value)
{
    Q_Q(SnoreCore);
    QString nk = normalizeKey(QString("%1-SnoreDefault").arg(key), LOCAL_SETTING);
    if (!m_settings->contains(nk)) {
        m_settings->setValue(nk, value);
    }
}

void SnoreCorePrivate::syncSettings()
{
    Q_Q(SnoreCore);
    QString newBackend = q->value("PrimaryBackend", LOCAL_SETTING).toString();
    if (!newBackend.isEmpty()) {
        QString oldBackend;
        if (m_notificationBackend) {
            oldBackend = m_notificationBackend->name();
            m_notificationBackend->deinitialize();
            m_notificationBackend = nullptr;
        }
        if (!setBackendIfAvailible(newBackend)) {
            snoreDebug(SNORE_WARNING) << "Failed to set new backend" << q->value("PrimaryBackend", LOCAL_SETTING).toString() << "restoring" << oldBackend;
            setBackendIfAvailible(oldBackend);
        }
    }

    auto types = SnorePlugin::types();
    types.removeOne(SnorePlugin::BACKEND);
    for(auto type : types) {
        for (auto &pluginName : m_pluginNames[type]) {
            SnorePlugin *plugin = m_plugins.value(pluginName);
            bool enable = m_plugins[pluginName]->value("Enabled", LOCAL_SETTING).toBool();
            if (!plugin->isInitialized() && enable) {
                plugin->initialize();
            } else if (plugin->isInitialized() && !enable) {
                plugin->deinitialize();
            }
        }
    }
}

void SnoreCorePrivate::setLocalSttingsPrefix(const QString &prefix)
{
    m_localSettingsPrefix = prefix;
    init();
    syncSettings();
}

QString SnoreCorePrivate::tempPath()
{
    static QTemporaryDir dir;
    return dir.path();
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
}

void SnoreCorePrivate::slotAboutToQuit()
{
    for (PluginContainer *p : PluginContainer::pluginCache(SnorePlugin::ALL)) {
        if (p->isLoaded()) {
            snoreDebug(SNORE_DEBUG) << "deinitialize" << p->name();
            p->load()->deinitialize();
        }
    }
}

static void loadTranslator()
{
    auto installTranslator = [](const QString & locale) {
        snoreDebug(SNORE_DEBUG) << locale;
        if (locale != "C") {
            QTranslator *translator = new QTranslator(qApp->instance());
            if (translator->load(locale, ":/lang/libsnore/")) {
                snoreDebug(SNORE_DEBUG) << "Using system locale:" << locale;
                snoreDebug(SNORE_DEBUG) << qApp->installTranslator(translator);
            } else {
                translator->deleteLater();
                QString fileName = QString(":/lang/libsnore/%1.qm").arg(locale);
                snoreDebug(SNORE_DEBUG) << "Failed to load translations for:" << locale << fileName << QFile::exists(fileName) ;
                return false;
            }
        }
        return true;
    };

    installTranslator("en");
    QLocale locale = QLocale::system();
    if (locale.name() != "en") {
        if (!installTranslator(locale.name())) {
            installTranslator(locale.bcp47Name());
        }
    }
}

static void registerMetaTypes()
{
    qRegisterMetaType<Notification>();
    qRegisterMetaType<Application>();
    qRegisterMetaType<SnorePlugin::PluginTypes>();
    qRegisterMetaTypeStreamOperators<SnorePlugin::PluginTypes>();
}

static void snoreStartup(){
    loadTranslator();
    registerMetaTypes();
}

Q_COREAPP_STARTUP_FUNCTION(snoreStartup)


