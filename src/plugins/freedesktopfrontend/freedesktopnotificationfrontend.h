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

#ifndef FREEDESKTOPNOTIFICATION_FRONTEND_H
#define FREEDESKTOPNOTIFICATION_FRONTEND_H
#include "core/interface.h"
#include <QtDBus>

class FreedesktopNotification_Frontend:public Notification_Frontend{
    Q_OBJECT
    Q_INTERFACES(Notification_Frontend)
public:
    FreedesktopNotification_Frontend(class SnoreServer *snore=0);
    ~FreedesktopNotification_Frontend();
    QString getImagefromHint(const class FreedesktopImageHint &img);

    void actionInvoked(Notification notification);
    void notificationClosed(Notification notification);
    uint Notify(const QString &app_name, uint replaces_id, const QString &app_icon, const QString &summary, const QString &body, const QStringList &actions, const QVariantMap &hints, int timeout);
    void CloseNotification( uint id );

    QStringList GetCapabilities();
    QString GetServerInformation(QString& vendor, QString& version, QString& specVersion);

signals:
    void NotificationClosed( uint id, uint reason );
    void ActionInvoked( uint id, const QString& actionKey );



};

#endif//FREEDESKTOPNOTIFICATION_FRONTEND_H
