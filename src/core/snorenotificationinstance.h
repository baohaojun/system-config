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

#ifndef SNORENOTIFICATIONINSTANCE_H
#define SNORENOTIFICATIONINSTANCE_H

#include "snore_exports.h"
#include "snoreserver.h"
#include "application.h"
#include "interface.h"

class SNORE_EXPORT  SnoreNotificationInstance:public Notification_Frontend
{
    Q_OBJECT
public:
    SnoreNotificationInstance ( const QString &appname, SnoreServer *parent, const QString &icon="" );
    ~SnoreNotificationInstance();
    void addAlert ( const QString &name,const QString &title = 0, const QString &icon="" );
    void registerWithBackends();
    void unregisterWithBackends();
	int notify ( const QString &alert,const QString &title,const QString &text,const QString &icon = 0,int timeout = 10, Notification::prioritys priority = Notification::NORMAL);
    void actionInvoked ( Notification notification );
    void notificationClosed ( Notification notification );

signals:
    void notificationActionInvoked ( const Notification &notification );

private:
    SnoreNotificationInstance();
    Application *_app;
    SnoreServer *_snore;


};

#endif // SNORENOTIFICATIONINSTANCE_H
