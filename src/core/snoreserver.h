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

#include <QStringList>

class SNORE_EXPORT SnoreServer:public QObject
{
    Q_OBJECT
public:
	static const QString version();
    static const QString snoreTMP();

public:
	 SnoreServer ( class QSystemTrayIcon *trayIcon=0 );
    void publicatePlugin ( const QString &fileName );
    void publicatePlugin ( SnorePlugin *plugin );


    int broadcastNotification ( Notification notification );
    void closeNotification ( Notification notification, const NotificationEnums::CloseReasons::closeReasons &reason );
    void notificationActionInvoked ( Notification notification );

    void addApplication ( Application *application );
    void applicationIsInitialized ( Application* application );
    void removeApplication ( const QString& appName );
    const ApplicationsList &aplications() const;

    const QStringList &primaryNotificationBackends() const;
    void setPrimaryNotificationBackend ( const QString &backend );
    const QString &primaryNotificationBackend();



private:
    ApplicationsList _applications;


    QHash<QString,Notification_Backend*> _notyfier;
    QStringList _primaryNotificationBackends;
    Notification_Backend * _notificationBackend;
    QHash<QString,SnorePlugin*> plugins;

    class QSystemTrayIcon *_trayIcon;


signals:
    void applicationInitialized ( Application* );
    void applicationRemoved ( Application* );
    void notify ( Notification noti );
    void actionInvoked( Notification );
    void closeNotify ( Notification );

};


#endif // SNORESERVER_H
