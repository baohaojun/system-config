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
#include <QTcpServer>
#include <QTcpSocket>
#include <QObject>
#include <QDir>
#include <QQueue>
#include <QTimer>
#include <QSharedPointer>
#include "application.h"
#include <QSharedPointer>
#include "interface.h"
#include <QDir>


class SNORE_EXPORT SnoreServer:public QObject
{
    Q_OBJECT
public:
    static const QString snoreTMP;


public:
    SnoreServer();
    void publicatePlugin(QObject* plugin);


    int broadcastNotification(QSharedPointer<Notification> notification);
    void closeNotification(QSharedPointer<Notification> notification);
    void notificationActionInvoked(QSharedPointer<Notification> notification);

    void addApplication(QSharedPointer<Application> application);
    bool applicationListAlertIsActive(const QString &applicationName,const QString &alertName);
    void addAlert(const QString &appName,const QString &alertName, const QString &alertTitle);
    void removeApplication(const QString& appName);
    const ApplicationsList &aplicationList() const;
    const QHash<QString,Notification_Backend*> &primaryNotificationBackends() const;
    void setNotificationBackend(Notification_Backend *backend);


    QHash<QString,QObject*> plugins;

private:
    ApplicationsList _applications;


    QHash<QString,Notification_Backend*> _notyfier;
    QHash<QString,Notification_Backend*> _primaryNotificationBackends;
    Notification_Backend * _notificationBackend;


signals:
    void applicationListChanged();
    void notify(QSharedPointer<Notification> noti);
    void closeNotify(int id);

};


#endif // SNORESERVER_H
