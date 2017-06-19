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

#ifndef NOTIFICATIONDATA_H
#define NOTIFICATIONDATA_H

#include "icon.h"
#include "notification.h"
#include "../hint.h"

#include <QPointer>
#include <QSharedData>
#include <QTimer>

namespace Snore
{
class SnorePlugin;

class SNORE_EXPORT NotificationData : public QSharedData
{
public:
    NotificationData(const Application &application, const Alert &alert, const QString &title, const QString &text, const Icon &icon,
                     int timeout, Notification::Prioritys priority);

    NotificationData(const Notification &old, const QString &title, const QString &text, const Icon &icon, int timeout, Notification::Prioritys priority);

    ~NotificationData();

    void setActionInvoked(const Action &action);

    void setCloseReason(Notification::CloseReasons r);

    QString resolveMarkup(const QString &string, Utils::MarkupFlags flags);

    void setBroadcasted();

    bool isBroadcasted() const;

    /**
     * Sets the source SnorePlugin.
     * @see source()
     */
    void setSource(SnorePlugin *soure);

    /**
     * Returns the source SnorePlugin.
     * This is used to prevent notification loops between the frontend and the backend.
     */
    const SnorePlugin *source() const;

    /**
     * Returns true if the source->name() and the target->name() are the same.
     * @todo rename
     */
    bool sourceAndTargetAreSimilar(const SnorePlugin *target);

private:
    Q_DISABLE_COPY(NotificationData)

    void stopTimeoutTimer();

    uint m_id;
    uint m_updateID;
    int m_timeout;
    Application m_application;
    Alert m_alert;
    QString m_title;
    QString m_text;
    Icon m_icon;
    Notification::Prioritys m_priority;
    Notification::CloseReasons m_closeReason = Notification::None;
    Action m_actionInvoked;
    QHash<int, Action> m_actions;
    Hint m_hints;
    Notification m_toReplace;
    QTimer *m_timeoutTimer = nullptr;
    QSet<const QObject *> m_activeIn;
    bool m_isBroadcasted = false;
    SnorePlugin *m_source = nullptr;

    static uint notificationCount;
    static uint m_idCount;

    friend class Notification;
    friend class SnoreCorePrivate;
};

}

#endif // NOTIFICATIONDATA_H
