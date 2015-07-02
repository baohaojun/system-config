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

#ifndef SNORECOREPRIVATE_H
#define SNORECOREPRIVATE_H

#include "snore.h"
#include "version.h"
#include "plugins/snorebackend.h"
#include "utils.h"

#include <QPointer>
#include <QApplication>

namespace Snore
{
class SNORE_EXPORT SnoreCorePrivate : public QObject
{
    Q_DECLARE_PUBLIC(SnoreCore)
    Q_OBJECT

public:
    /**
     * @brief tempPath
     * @return the path of a QTemporaryDir, this location is used to store images.
     */
    static QString tempPath();

public:
    static SnoreCorePrivate *instance();
    ~SnoreCorePrivate();
    Application defaultApplication();

    bool setBackendIfAvailible(const QString &backend);

    /**
     *
     * @return whether the backend can update a notification
     */
    bool primaryBackendCanUpdateNotification() const;

    bool initPrimaryNotificationBackend();

    void syncSettings();

    QString normalizeKey(const QString &key, SettingsType type) const
    {
        return Snore::Utils::normalizeSettingsKey(key, type, m_localSettingsPrefix);
    }

    void setLocalSttingsPrefix(const QString &prefix);

    void init();

    /**
     * Set a default value wich can be overritten by a client application call to SnoreCore::setDefaultValue()
     */
    void setDefaultValueIntern(const QString &key, const QVariant &value);

private Q_SLOTS:
    //TODO: find a better solutinon for the slots in this section
    friend class Snore::SnoreBackend;
    void slotNotificationActionInvoked(Notification notification);
    void slotNotificationDisplayed(Notification notification);
    void slotNotificationClosed(Snore::Notification);
    void slotAboutToQuit();

Q_SIGNALS:
    void applicationRegistered(const Snore::Application &);
    void applicationDeregistered(const Snore::Application &);
    void notify(Snore::Notification noti);
    void notificationDisplayed(Snore::Notification notification);

private:
    SnoreCorePrivate();
    SnoreCore *q_ptr;

    QHash<QString, Application> m_applications;

    QHash<SnorePlugin::PluginTypes, QStringList> m_pluginNames;
    QHash<QString, SnorePlugin *> m_plugins;

    QPointer<SnoreBackend> m_notificationBackend;

    Application m_defaultApp;

    QString m_localSettingsPrefix;

    QSettings *m_settings;

    QHash<uint, Snore::Notification> m_activeNotifications;
    friend class Snore::Notification;

};
}

#endif // SNORECOREPRIVATE_H
