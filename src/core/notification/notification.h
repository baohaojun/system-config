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

#ifndef NOTIFICATION_H
#define NOTIFICATION_H
#include "../snore_exports.h"
#include "icon.h"

#include "notificationenums.h"

#include <QVariant>
#include <QDebug>

namespace Snore{

class SNORE_EXPORT Notification
{
public:
    static int DefaultTimeout;
    static QString toPlainText ( const QString &string );

    class Action
    {
    public:
        Action(int id,QString name):id(id),name(name){}
        int id;
        QString name;
    };

public:
    Notification ();
    Notification (const QString &application,const QString &alert,const QString &title,const QString &text,const SnoreIcon &icon,int timeout=10,uint id = 0, NotificationEnums::Prioritys::prioritys priority = NotificationEnums::Prioritys::NORMAL );
    Notification ( const Notification &other );
    ~Notification();
    Notification &operator=(const Notification& other);

    const uint &id() const;
    //timeout in seconds
    //0 means sticky
    const int &timeout() const;

    const Action* actionInvoked() const;
    void setSource(class SnoreFrontend *source)const;
    class SnoreFrontend *source() const;
    const QString &application() const;
    const QString &title() const;
    const QString &text() const;
    const SnoreIcon &icon() const;
    const QString &alert() const;
    void setSticky();
    bool sticky() const;
    const NotificationEnums::Prioritys::prioritys &priority() const;
    const QHash<int,Action*> &actions() const;
    void addAction(Action *a);
    const NotificationEnums::CloseReasons::closeReasons &closeReason();
    void setCloseReason(const NotificationEnums::CloseReasons::closeReasons &r);
    const QVariant hint ( const QString &key ) const;
    bool hintExists ( const QString &key );
    void insertHint ( const QString &key,const QVariant &val );

    bool isValid() const;

//protected://TODO::make only accesable from a backend
    void setActionInvoked ( Action *action );
    void setActionInvoked ( const int &actionID);


private:
    static uint m_idCount;
    class NotificationData;
    NotificationData* d;
    static int notificationCount;
    static int notificationMetaID;

};

}

Q_DECLARE_METATYPE(Snore::Notification)

QDataStream &operator<< ( QDataStream & stream, const Snore::Notification & noti );

inline QDebug operator<< ( QDebug debug, const Snore::Notification &noti )
{
    debug << "Snore::Notification(" << noti.title() << ", " << noti.text() << "," << noti.id() << ")" ;
    return debug.maybeSpace();
}

QDataStream &operator<< ( QDataStream & stream, const Snore::Notification::Action & action);
#endif // NOTIFICATION_H
