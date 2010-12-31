/****************************************************************************************
 * Copyright (c) 2010 Patrick von Reth <patrick.vonreth@gmail.com>                      *
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
#include "interface.h"


class SNORE_EXPORT SnoreServer:public QObject
{
    Q_OBJECT
public:
	static const QString version();
    static const QString snoreTMP();
    static void cleanupTMP();


public:
    SnoreServer ( class QSystemTrayIcon *trayIcon=0 );
    void publicatePlugin ( const QString &fileName );
    void publicatePlugin ( SnorePlugin *plugin );


    int broadcastNotification ( QSharedPointer<Notification> notification );
    void closeNotification ( QSharedPointer<Notification> notification );
    void notificationActionInvoked ( QSharedPointer<Notification> notification );

    void addApplication ( Application *application );
    void applicationIsInitialized ( Application* application );
    void removeApplication ( const QString& appName );
    const ApplicationsList &aplications() const;

    const QHash<QString,Notification_Backend*> &primaryNotificationBackends() const;
    void setPrimaryNotificationBackend ( Notification_Backend *backend );

    class SnoreNotificationInstance *defaultNotificationInterface();

    QHash<QString,SnorePlugin*> plugins;

private:
    ApplicationsList _applications;
    class SnoreNotificationInstance *_defaultNotificationInterface;


    QHash<QString,Notification_Backend*> _notyfier;
    QHash<QString,Notification_Backend*> _primaryNotificationBackends;
    Notification_Backend * _notificationBackend;

    class QSystemTrayIcon *_trayIcon;


signals:
    void applicationInitialized ( Application* );
    void applicationRemoved ( Application* );
    void notify ( QSharedPointer<Notification> noti );
    void closeNotify ( QSharedPointer<Notification> );

};


#endif // SNORESERVER_H
