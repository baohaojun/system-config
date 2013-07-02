/****************************************************************************************
 * Copyright (c) 2010-2012 Patrick von Reth <patrick.vonreth@gmail.com>                 *
 *                                                                                      *
 * This program is free software; you can redistribute it and/or modify it under        *
 * the terms of the GNU General Public License as published by the Free Software        *
 * Foundation; either version 2 of the License, or (at your option) any later           *
 * version.                                                                             *
 *                                                                                      *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY      *
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A      *
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.             *
 *                                                                                      *
 * You should have received a copy of the GNU General Public License along with         *
 * this program.  If not, see <http://www.gnu.org/licenses/>.                           *
 ****************************************************************************************/

#ifndef SNORESERVER_H
#define SNORESERVER_H

#include "snore_exports.h"
#include "application.h"
#include "plugins/plugincontainer.h"
#include "notification/notification.h"

#include <QStringList>

class QSystemTrayIcon;
class QDir;
class QSettings;


namespace Snore{
class SNORE_EXPORT SnoreCore:public QObject
{
    Q_OBJECT
public:
    static const QString version();
    static const QString snoreTMP();
    static void updatePluginCache();
    static const QDir &pluginDir();
    static void setPluginDir(const QString &path = "");

public:
    SnoreCore (QSystemTrayIcon *trayIcon = NULL );
    void loadPlugins ( PluginContainer::PluginTypes types );


    uint broadcastNotification ( Notification notification );
    void closeNotification ( Notification notification, const NotificationEnums::CloseReasons::closeReasons &reason );
    void notificationActionInvoked ( Notification notification );

    void addApplication ( Application *application );
    void applicationIsInitialized ( Application* application );
    void removeApplication ( const QString& appName );
    const ApplicationsList &aplications() const;

    const QStringList &notificationBackends() const;
    const QStringList &notificationFrontends() const;
    const QStringList &secondaryNotificationBackends() const;

    void setPrimaryNotificationBackend ( const QString &backend );
    const QString &primaryNotificationBackend();
    QSystemTrayIcon *trayIcon();

    Notification getActiveNotificationByID(uint id);



private:
    static QHash<QString,PluginContainer*> pluginCache();
    static QSettings *cacheFile();

    static QHash<QString,PluginContainer*> s_pluginCache;
    static QDir *s_pluginDir;

    ApplicationsList m_applications;
    QHash<uint,Notification> m_activeNotifications;


    QStringList m_notificationBackends;
    QStringList m_Frontends;
    QStringList m_secondaryNotificationBackends;
    QStringList m_plugins;

    QPointer<SnoreBackend> m_notificationBackend;

    QSystemTrayIcon *m_trayIcon;


signals:
    void applicationInitialized( Snore::Application* );
    void applicationRemoved( Snore::Application* );
    void notify( Snore::Notification noti );
    void actionInvoked( Snore::Notification );
    void closeNotify( Snore::Notification );

};

}

#endif // SNORESERVER_H
