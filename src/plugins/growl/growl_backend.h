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

#include <string>

class Growl_Backend:public Notification_Backend
{
    Q_OBJECT
    Q_INTERFACES(Notification_Backend)
public:
    Growl_Backend(class SnoreServer *snore=0);
    ~Growl_Backend();
    bool isPrimaryNotificationBackend(){return true;}
	static void gntpCallback(const int &id,const std::string &reason,const std::string &data);
private:
	//a static instance for the static callback methode
	static Growl_Backend *instance;
    uint _id;
    QHash<QString,class gntp*> _applications;

public slots:
    void registerApplication(Application *application);
    void unregisterApplication(class Application *application);
    uint notify(Notification notification);
    void closeNotification(Notification notification);
};


#endif // GROWL_BACKEND_H
