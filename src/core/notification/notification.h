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

#ifndef NOTIFICATION_H
#define NOTIFICATION_H
#include "../snore_exports.h"
#include "icon.h"
#include "notificationenums.h"
#include "notificationaction.h"
#include "../hint.h"
#include "../application.h"

#include <QVariant>
#include <QDebug>



namespace Snore{

class NotificationData;

class SNORE_EXPORT Notification
{
    friend class NotificationData;
public:
    Notification();
    explicit Notification(const Application &application,const Alert &alert,const QString &title,const QString &text,const Icon &icon,int timeout=10, NotificationEnums::Prioritys::prioritys priority = NotificationEnums::Prioritys::NORMAL );
    Notification(const Notification &other );
    Notification &operator=(const Notification &other);
    ~Notification();

    uint id() const;
    //timeout in seconds
    //0 means sticky
    const int &timeout() const;

    void setUpdateID(uint id);
    const uint &updateID() const;

    const Action &actionInvoked() const;
    Application application() const;
    QString title() const;
    QString text() const;
    const Icon &icon() const;
    Alert alert() const;
    void setSticky();
    bool sticky() const;
    const NotificationEnums::Prioritys::prioritys &priority() const;
    const QHash<int, Action> &actions() const;
    void addAction(const Action &a);
    const NotificationEnums::CloseReasons::closeReasons &closeReason();
    Hint &hints();

    void setSilent(bool silent);

    bool isValid() const;

    NotificationData *data();


    static int defaultTimeout();
    static void setDefaultTimeout(int defaultTimeout);

private:
    QExplicitlySharedDataPointer<NotificationData> d;

    static int m_defaultTimeout;


};

}

Q_DECLARE_METATYPE(Snore::Notification)

QDataStream &operator<< ( QDataStream & stream, const Snore::Notification & noti );

inline QDebug operator<< ( QDebug debug, const Snore::Notification &noti )
{
    if(noti.isValid())
    {
        debug << "Snore::Notification(" << noti.title() << ", " << noti.text() << "," << noti.id() << ")" ;
    }
    else
    {
        debug << "Snore::Notification(0x00)" ;
    }
    return debug.maybeSpace();
}


#endif // NOTIFICATION_H
