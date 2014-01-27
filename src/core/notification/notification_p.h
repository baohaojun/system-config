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

#ifndef NOTIFICATIONDATA_H
#define NOTIFICATIONDATA_H


#include "icon.h"
#include "notification.h"
#include "../hint.h"

#include <QSharedData>
#include <QTimer>
#include <QPointer>


namespace Snore{


class SNORE_EXPORT NotificationData : public QSharedData
{

    friend class Notification;
public:
    NotificationData ( const Application &application,const Alert &alert,const QString &title,const QString &text,const Icon &icon,
                       int timeout,Notification::Prioritys priority );

    NotificationData(const Notification &old,const QString &title,const QString &text,const Icon &icon,int timeout, Notification::Prioritys priority);


    ~NotificationData();

    void setSource(class SnoreFrontend *source);
    class SnoreFrontend *source() const;

    void setActionInvoked( const Action &action );
    void setActionInvoked( const int &actionID);


    void setCloseReason(Notification::CloseReasons r);


    QTimer *timeoutTimer();

private:
    Q_DISABLE_COPY(NotificationData)

    uint m_id;
    uint m_updateID;
    int m_timeout;
    SnoreFrontend *m_source;
    Application m_application;
    Alert m_alert;
    QString m_title;
    QString m_text;
    Icon m_icon;
    Notification::Prioritys m_priority;
    Notification::CloseReasons m_closeReason;
    Action m_actionInvoked;
    QHash<int,Action> m_actions;
    Hint m_hints;
    Notification m_toReplace;
    QPointer<QTimer> m_timeoutTimer;



    static uint notificationCount;
    static uint m_idCount;
    static int notificationMetaID;

};

}

#endif // NOTIFICATIONDATA_H
