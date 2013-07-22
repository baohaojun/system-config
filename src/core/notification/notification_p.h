#ifndef NOTIFICATIONDATA_H
#define NOTIFICATIONDATA_H


#include "notification/icon.h"
#include "notification/NotificationEnums.h"

#include <QSharedData>

namespace Snore{

class NotificationData : public QSharedData
{

public:
    NotificationData ( const QString &application,const QString &alert,const QString &title,const QString &text,const SnoreIcon &icon,
                       int timeout,NotificationEnums::Prioritys::prioritys priority ):
        m_id ( m_idCount++ ),
        m_updateID(0),
        m_timeout ( timeout ),
        m_source ( NULL),
        m_application ( application ),
        m_alert ( alert ),
        m_title ( title ),
        m_text ( text ),
        m_icon ( icon ),
        m_priority(priority),
        m_closeReason(NotificationEnums::CloseReasons::NONE)
    {
        notificationCount++;
        qDebug()<< "Creating Notification: ActiveNotifications" << notificationCount << "id" << m_id;
    }

    ~NotificationData()
    {
        notificationCount--;
        qDebug() << "Deleting Notification: ActiveNotifications" << notificationCount << "id" << m_id;
    }


    uint m_id;
    uint m_updateID;
    int m_timeout;
    Notification::Action *m_actionInvoked;
    SnoreFrontend *m_source;
    QString m_application;
    QString m_alert;
    QString m_title;
    QString m_text;
    SnoreIcon m_icon;
    NotificationEnums::Prioritys::prioritys m_priority;
    NotificationEnums::CloseReasons::closeReasons m_closeReason;
    QHash<int,Notification::Action*> m_actions;
    QVariantHash m_hints;

    static uint notificationCount;
    static uint m_idCount;
    static int notificationMetaID;

private:
    Q_DISABLE_COPY(NotificationData)

};

}

#endif // NOTIFICATIONDATA_H
