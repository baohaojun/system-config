/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2014  Patrick von Reth <vonreth@kde.org>


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
#include "notification/notification.h"
#include "plugins/plugins.h"
#include "hint.h"

#include <QStringList>
#include <QTextDocument>
#include <QTextDocumentFragment>

class QSystemTrayIcon;


namespace Snore{
class SnoreCorePrivate;

class SNORE_EXPORT SnoreCore : public QObject
{
    Q_DECLARE_PRIVATE(SnoreCore)
    Q_OBJECT
public:
    SnoreCore (QSystemTrayIcon *trayIcon = NULL );
    ~SnoreCore();
    void loadPlugins ( SnorePlugin::PluginTypes types );


    void broadcastNotification( Notification notification );

    void registerApplication(const Application &application );
    void deregisterApplication(const Application &application );

    const QHash<QString, Application> &aplications() const;

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

    const SnoreCorePrivate *d();


signals:
    void actionInvoked( Snore::Notification );
    void notificationClosed(Snore::Notification );

private:
    SnoreCorePrivate *d_ptr;


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
