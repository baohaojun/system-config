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

#ifndef SNARL_BACKEND_H
#define SNARL_BACKEND_H
#include "core/interface.h"
#include "SnarlInterface.h"

#include <QWidget>

class SnarlWidget;

class Snarl_Backend:public Notification_Backend
{
    Q_OBJECT
    Q_INTERFACES(Notification_Backend)
public:
    Snarl_Backend(class SnoreServer *snore=0);
    ~Snarl_Backend();
    bool isPrimaryNotificationBackend();
	QHash<int,QSharedPointer<Notification> >* activeNotifications;

private:
	SnarlWidget* winIDWidget;

    QHash<QString,Snarl::V41::SnarlInterface*> _applications;
    Snarl::V41::SnarlInterface* _defautSnarlinetrface;

public slots:
    void registerApplication(Application *application);
    void unregisterApplication(class Application *application);
    int notify(QSharedPointer<Notification>notification);
    void closeNotification(QSharedPointer<Notification> notification);

};

class SnarlWidget:public QWidget
{
	Q_OBJECT
public:
	SnarlWidget(Snarl_Backend* snarl);
	bool winEvent( MSG * message, long * result );

private:
	Snarl_Backend* _snarl;

};



#endif // SNARL_BACKEND_H
