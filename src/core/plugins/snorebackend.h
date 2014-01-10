/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013  Patrick von Reth <vonreth@kde.org>


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

#ifndef SNORE_BACKEND_H
#define SNORE_BACKEND_H
#include "../snore_exports.h"
#include "plugins.h"
#include "../notification/notification.h"
#include "../snore.h"

#include <QPointer>
#include <QFlag>

namespace Snore{

class SNORE_EXPORT SnoreBackend : public SnorePlugin
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnorePlugin)
public:
    SnoreBackend(const  QString &name, bool canCloseNotification, bool supportsRichtext );
    virtual ~SnoreBackend();
    virtual bool init(SnoreCore *snore);

    void requestCloseNotification( Snore::Notification notification,NotificationEnums::CloseReasons::closeReasons reason );

    Snore::Notification getActiveNotificationByID(uint id);

    bool canCloseNotification();
    bool supportsRichtext();

signals:
    void closeNotification( Snore::Notification );


public slots:
    virtual void slotRegisterApplication ( Snore::Application *application ) = 0;
    virtual void slotUnregisterApplication ( Snore::Application *application ) = 0;
    virtual void slotNotify ( Snore::Notification notification ) = 0;
    virtual void slotCloseNotification ( Snore::Notification notification );

protected:
    bool m_canCloseNotification;
    bool m_supportsRichtext;
    void closeNotification(Snore::Notification,Snore::NotificationEnums::CloseReasons::closeReasons);


private:
    QHash<uint,Notification> m_activeNotifications;

    void addActiveNotification(Notification n);
    friend void SnoreCore::broadcastNotification(Notification notification);


};

}
Q_DECLARE_INTERFACE ( Snore::SnoreBackend,
                      "org.Snore.NotificationBackend/1.0" )

namespace Snore{
class SnoreCore;

class SNORE_EXPORT SnoreSecondaryBackend:public SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnorePlugin Snore::SnoreBackend)
public:
    SnoreSecondaryBackend(const  QString &name, bool supportsRhichtext);
    virtual ~SnoreSecondaryBackend();
    virtual bool init(SnoreCore *snore);

};

}

Q_DECLARE_INTERFACE ( Snore::SnoreSecondaryBackend,
                      "org.Snore.SecondaryNotificationBackend/1.0" )

#endif//SNORE_BACKEND_H
