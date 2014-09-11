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

#ifndef OSXNOTIFICATIONCENTER_H
#define OSXNOTIFICATIONCENTER_H

#include "core/plugins/snorebackend.h"

class OSXNotificationCenter : public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0")

public:
    OSXNotificationCenter();
    ~OSXNotificationCenter();
    virtual bool initialize(Snore::SnoreCore *snore);

public slots:
    virtual void slotNotify(Snore::Notification notification);
};

#endif // OSXNOTIFICATIONCENTER_H
