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
#include "../hint.h"

#include <QVariant>
#include <QDebug>



namespace Snore{

class NotificationData;

class SNORE_EXPORT Notification
{
    friend class NotificationData;
public:
    class SNORE_EXPORT Action
    {
    public:
        Action();
        Action(int id,QString name);
        Action(const Action &other);

        int id() const;
        QString name() const;
        bool isValid() const;

    private:
        int m_id;
        QString m_name;
    };


public:
    Notification ();
    Notification (const QString &application,const QString &alert,const QString &title,const QString &text,const Icon &icon,int timeout=10, NotificationEnums::Prioritys::prioritys priority = NotificationEnums::Prioritys::NORMAL );
    Notification ( const Notification &other );
    Notification &operator=(const Notification &other);
    ~Notification();

    const uint &id() const;
    //timeout in seconds
    //0 means sticky
    const int &timeout() const;

    void setUpdateID(uint id);
    const uint &updateID() const;

    const Action &actionInvoked() const;
    void setSource(class SnoreFrontend *source);
    class SnoreFrontend *source() const;
    const QString &application() const;
    const QString &title() const;
    const QString &text() const;
    const Icon &icon() const;
    const QString &alert() const;
    void setSticky();
    bool sticky() const;
    const NotificationEnums::Prioritys::prioritys &priority() const;
    const QHash<int, Action> &actions() const;
    void addAction(const Action &a);
    const NotificationEnums::CloseReasons::closeReasons &closeReason();
    void setCloseReason(const NotificationEnums::CloseReasons::closeReasons &r);
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

QDataStream &operator<< ( QDataStream & stream, const Snore::Notification::Action & action);
#endif // NOTIFICATION_H
