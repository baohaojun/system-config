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

#ifndef SNORE_BACKEND_H
#define SNORE_BACKEND_H
#include "../snore_exports.h"
#include "../notification/notification.h"
#include "plugins.h"

#include <QPointer>
#include <QFlag>
#include <QtCore>

namespace Snore{
class SnoreServer;

class SNORE_EXPORT SnoreBackend:public SnorePlugin
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnorePlugin)
public:
    SnoreBackend ( QString name );
    virtual ~SnoreBackend();
    virtual bool init(SnoreServer *snore);



public slots:
    virtual void registerApplication ( Snore::Application *application ) =0;
    virtual void unregisterApplication ( Snore::Application *application ) =0;
    virtual uint notify ( Snore::Notification notification ) =0;
    virtual void closeNotification ( Snore::Notification notification ) =0;



};

}
Q_DECLARE_INTERFACE ( Snore::SnoreBackend,
                      "org.Snore.NotificationBackend/1.0" )

namespace Snore{
class SnoreServer;

class SNORE_EXPORT SnoreSecondaryBackend:public SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnorePlugin Snore::SnoreBackend)
public:
    SnoreSecondaryBackend(const  QString &name);
    virtual ~SnoreSecondaryBackend();
    virtual bool init(SnoreServer *snore);

};

}

Q_DECLARE_INTERFACE ( Snore::SnoreSecondaryBackend,
                      "org.Snore.SecondaryNotificationBackend/1.0" )

#endif//SNORE_BACKEND_H
