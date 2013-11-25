/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013  Patrick von Reth <vonreth@kde.org>


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

#ifndef SNORESERVER_H
#define SNORESERVER_H

#include "snore_exports.h"
#include "application.h"
#include "plugins/plugincontainer.h"
#include "notification/notification.h"
#include "hint.h"

#include <QStringList>
#include <QTextDocument>
#include <QTextDocumentFragment>

class QSystemTrayIcon;
class QDir;


namespace Snore{
class SNORE_EXPORT SnoreCore : public QObject
{
    Q_OBJECT
public:
    static const QString snoreTMP();
    static const QDir &pluginDir();

public:
    SnoreCore (QSystemTrayIcon *trayIcon = NULL );
    void loadPlugins ( PluginContainer::PluginTypes types );


    void broadcastNotification( Notification notification );
    void notificationActionInvoked ( Notification notification );

    void addApplication ( Application *application );
    void applicationIsInitialized ( Application* application );
    void removeApplication ( const QString& appName );
    const ApplicationsList &aplications() const;

    const QStringList &notificationBackends() const;
    const QStringList &notificationFrontends() const;
    const QStringList &secondaryNotificationBackends() const;

    bool setPrimaryNotificationBackend( const QString &backend );
    bool setPrimaryNotificationBackend();
    const QString primaryNotificationBackend() const;
    QSystemTrayIcon *trayIcon();

    Notification getActiveNotificationByID(uint id);

    void requestCloseNotification(Notification,NotificationEnums::CloseReasons::closeReasons);

    bool primaryBackendSupportsRichtext();

    Hint &hints();

signals:
    void applicationInitialized( Snore::Application* );
    void applicationRemoved( Snore::Application* );
    void notify( Snore::Notification noti );
    void actionInvoked( Snore::Notification );
    void notificationClosed(Snore::Notification );

private slots:
    void slotNotificationClosed(Snore::Notification);


private:
    Hint m_hints;

    ApplicationsList m_applications;


    QStringList m_notificationBackends;
    QStringList m_Frontends;
    QStringList m_secondaryNotificationBackends;
    QStringList m_plugins;

    QPointer<SnoreBackend> m_notificationBackend;

    QSystemTrayIcon *m_trayIcon;
};


static inline QString toPlainText ( const QString &string)
{
        if(Qt::mightBeRichText(string))
        {
            return QTextDocumentFragment::fromHtml(string).toPlainText();
        }
        else
        {
            return string;
        }
}

}

#endif // SNORESERVER_H
