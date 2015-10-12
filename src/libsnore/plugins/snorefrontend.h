/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2015  Hannah von Reth <vonreth@kde.org>

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

#ifndef SNORE_FRONTEND_H
#define SNORE_FRONTEND_H
#include "snore_exports.h"
#include "../notification/notification.h"
#include "plugins.h"

namespace Snore
{
class SnorePlugin;

class SNORE_EXPORT SnoreFrontend: public SnorePlugin
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnorePlugin)
public:
    SnoreFrontend();
    virtual ~SnoreFrontend();

public Q_SLOTS:
    virtual void slotActionInvoked(Snore::Notification notification);
    virtual void slotNotificationClosed(Snore::Notification notification);
};

}

Q_DECLARE_INTERFACE(Snore::SnoreFrontend,
                    "org.Snore.NotificationFrontend/1.0")

#endif//SNORE_FRONTEND_H
