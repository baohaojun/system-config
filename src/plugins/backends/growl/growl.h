/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2014  Patrick von Reth <vonreth@kde.org>


    SnoreNotify is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SnoreNotify is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with SnoreNotify.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef GROWL_BACKEND_H
#define GROWL_BACKEND_H
#include "core/plugins/snorebackend.h"

#include <string>

class Growl:public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0")

public:
    Growl();
    ~Growl();    
    virtual bool init(Snore::SnoreCore *snore);
    
    static void gntpCallback(const int &id,const std::string &reason,const std::string &data);

private:
	//a static instance for the static callback methode
    static Growl *s_instance;
    uint m_id;
    QHash<QString,class gntp*> m_applications;
    gntp *m_defaultGNTP;

public slots:
    void slotRegisterApplication(Snore::Application *application);
    void slotUnregisterApplication(Snore::Application *application);
    void slotNotify(Snore::Notification notification);
};


#endif // GROWL_BACKEND_H
