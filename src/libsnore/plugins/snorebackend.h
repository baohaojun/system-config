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

#ifndef SNORE_BACKEND_H
#define SNORE_BACKEND_H
#include "libsnore/snore_exports.h"
#include "libsnore/plugins/plugins.h"
#include "libsnore/notification/notification.h"
#include "libsnore/snore.h"

namespace Snore
{

class SNORE_EXPORT SnoreBackend : public SnorePlugin
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnorePlugin)
public:
    SnoreBackend() = default;
    virtual ~SnoreBackend();
    virtual bool initialize() override;
    virtual bool deinitialize() override;

    void requestCloseNotification(Snore::Notification notification, Notification::CloseReasons reason);

    virtual bool canCloseNotification() const;
    virtual bool canUpdateNotification() const;

Q_SIGNALS:
    void notificationClosed(Snore::Notification);

public Q_SLOTS:
    virtual void slotRegisterApplication(const Snore::Application &application);
    virtual void slotDeregisterApplication(const Snore::Application &application);
    virtual void slotNotify(Snore::Notification notification) = 0;
    virtual void slotCloseNotification(Snore::Notification notification);

protected Q_SLOTS:
    void slotNotificationDisplayed(Notification notification);
    void slotNotificationActionInvoked(Notification notification, const Action &action = Action());

protected:
    void closeNotification(Snore::Notification, Snore::Notification::CloseReasons);


};

}
Q_DECLARE_INTERFACE(Snore::SnoreBackend,
                    "org.Snore.NotificationBackend/1.0")

namespace Snore
{
class SnoreCore;

class SNORE_EXPORT SnoreSecondaryBackend : public SnorePlugin
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnorePlugin Snore::SnorePlugin)
public:
    SnoreSecondaryBackend() = default;
    virtual ~SnoreSecondaryBackend();
    virtual bool initialize();
    virtual bool deinitialize();

public Q_SLOT:
    virtual void slotNotify(Snore::Notification notification);
    virtual void slotNotificationDisplayed(Snore::Notification notification);

};

}

Q_DECLARE_INTERFACE(Snore::SnoreSecondaryBackend,
                    "org.Snore.SecondaryNotificationBackend/1.0")

#endif//SNORE_BACKEND_H
