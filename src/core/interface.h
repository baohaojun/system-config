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
#include "notification.h"
#include <QObject>
#include <QTcpSocket>
#include <QSharedPointer>
#include <QPointer>

class SNORE_EXPORT SnorePlugin{
public:
    virtual ~SnorePlugin(){}; 
    virtual void setSnore(class SnoreServer* snore);
    virtual class SnoreServer* getSnore();
private:
    QPointer<class SnoreServer> snore;

};

class SNORE_EXPORT Notification_Backend:public QObject,public SnorePlugin
{
    Q_OBJECT
public:
    virtual ~Notification_Backend() {}
    virtual bool isPrimaryNotificationBackend()=0;

public slots:
    virtual int notify(QSharedPointer<Notification> notification)=0;
    virtual void closeNotification(int id)=0;

    //    virtual void update

};


class SNORE_EXPORT Notification_Frontend:public SnorePlugin{
public:
    virtual ~Notification_Frontend() {}
     virtual void actionInvoked(QSharedPointer<Notification> notification)=0;
     virtual void notificationClosed(QSharedPointer<Notification> notification)=0;
};



Q_DECLARE_INTERFACE(SnorePlugin,
                    "org.Snore.SnorePlugin/1.0")
Q_DECLARE_INTERFACE(Notification_Frontend,
                    "org.Snore.NotificationFrontend/1.0")
Q_DECLARE_INTERFACE(Notification_Backend,
                    "org.Snore.NotificationBackend/1.0")


#endif//INTERFACE_H
