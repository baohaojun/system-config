/****************************************************************************************
 * Copyright (c) 2010-2012 Patrick von Reth <patrick.vonreth@gmail.com>                 *
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
#include "core/plugins/snorebackend.h"

#include <string>

class Growl:public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
public:
    Growl();
    ~Growl();
    static void gntpCallback(const int &id,const std::string &reason,const std::string &data);

private:
	//a static instance for the static callback methode
    static Growl *s_instance;
    uint m_id;
    QHash<QString,class gntp*> m_applications;

public slots:
    void slotRegisterApplication(Snore::Application *application);
    void slotUnregisterApplication(Snore::Application *application);
    void slotNotify(Snore::Notification notification);
    bool slotCloseNotification(Snore::Notification notification);
};


#endif // GROWL_BACKEND_H
