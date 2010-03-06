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

#ifndef GROWL_BACKEND_H
#define GROWL_BACKEND_H
#include "core/interface.h"
class Growl_Backend:public Notification_Backend
{
    Q_OBJECT
    Q_INTERFACES(Notification_Backend);
public:
    Growl_Backend();
    ~Growl_Backend();
    bool isPrimaryNotificationBackend(){return true;}
private:
    uint id;
    class Growl *growl;
public slots:
    int notify(QSharedPointer<Notification>notification);
    void closeNotification(int nr);
};

#endif // GROWL_BACKEND_H
