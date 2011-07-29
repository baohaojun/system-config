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

#ifndef INTERFACE_H
#define INTERFACE_H
#include "snore_exports.h"
#include "notification/notification.h"


class SNORE_EXPORT SnorePlugin:public QObject
{
    Q_OBJECT
public:
    SnorePlugin ( QString name,class SnoreServer *snore=0 );
    virtual ~SnorePlugin();
    virtual void setSnore ( class SnoreServer* snore );
    virtual class SnoreServer* snore();
    const QString &name() const;
private:
    SnorePlugin() {}
    QString _name;
    class SnoreServer *_snore;


};

Q_DECLARE_INTERFACE ( SnorePlugin,
                      "org.Snore.SnorePlugin/1.0" )

class SNORE_EXPORT Notification_Backend:public SnorePlugin
{
    Q_OBJECT
    Q_INTERFACES(SnorePlugin)
public:
    Notification_Backend ( QString name,class SnoreServer *snore=0 );
    virtual ~Notification_Backend();
    virtual bool isPrimaryNotificationBackend() =0;

protected:
	QHash<uint,Notification> activeNotifications;

public slots:
    virtual void registerApplication ( class Application *application ) =0;
    virtual void unregisterApplication ( class Application *application ) =0;
    virtual int notify ( Notification notification ) =0;
    virtual void closeNotification (Notification notification ) =0;

    //    virtual void update

};


class SNORE_EXPORT Notification_Frontend:public SnorePlugin
{
    Q_OBJECT
    Q_INTERFACES(SnorePlugin)
public:
    Notification_Frontend ( QString name,class SnoreServer *snore=0 );
    virtual ~Notification_Frontend();
    virtual void actionInvoked (Notification notification )=0;
    virtual void notificationClosed ( Notification notification )=0;

protected:
	QHash<uint,Notification> activeNotifications;
};




Q_DECLARE_INTERFACE ( Notification_Frontend,
                      "org.Snore.NotificationFrontend/1.0" )
Q_DECLARE_INTERFACE ( Notification_Backend,
                      "org.Snore.NotificationBackend/1.0" )


#endif//INTERFACE_H
