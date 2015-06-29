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
#include "libsnore/snore_exports.h"
#include "icon.h"
#include "notificationaction.h"
#include "libsnore/hint.h"
#include "libsnore/application.h"
#include "libsnore/utils.h"

#include <QDebug>

namespace Snore
{

class NotificationData;
class SnorePlugin;
/**
 *  Notification contains all relevant data to notify the user.
 *  Notification uses a shared datamodel, it's content is never copied and automatically released.
 *
 * @author Patrick von Reth \<vonreth at kde.org\>
 */

class SNORE_EXPORT Notification
{
    friend class NotificationData;
public:
    /**
     * The reason why the Notification was closed.
     */
    enum CloseReasons {
        /**
         * The default value, the notification was not closed.
         */
        NONE = 0,

        /**
         * The Notification was closed becaouse it timed out.
         */
        TIMED_OUT = 1,

        /**
         * The Notification was dismissed by the user, close button.
         */
        DISMISSED = 2,

        /**
         * The Notification was activated, an action was invoked.
         * @see actionInvoked()
         */
        ACTIVATED = 3,

        /**
         * @deprecated same as ACTIVATED
         */
        CLOSED = 3,

        /**
         * The notification was replaced by an update.
         * This value will be used if a notification backend does not support updating.
         *
         */
        REPLACED = 4
    };
    Q_ENUMS(CloseReasons)

    /**
     * The Priority for the Notification.
     * Some notification systems support this flag to filter notifications or indicate different prioritys by color.
     */
    enum Prioritys {
        /**
         * Indicates the lowes priority. The backend might ignore the notification.
         */
        LOWEST = -2,

        /**
         * Indicates a low priority.
         */
        LOW = -1,

        /**
         * The default priority.
         */
        NORMAL = 0,

        /**
         * Indicates a priority above the normal level.
         */
        HIGH = +1,

        /**
         * Indicates a emegency priority, the notifications is sticky and should be acknowlegded.
         */
        EMERGENCY = +2
    };

    Notification();
    /**
     * Creates a new Notification.
     * @param application the application emitting the Notification
     * @param alert the associated alert
     * @param title the title
     * @param text the text body
     * @param icon the icon
     * @param timeout the timeout
     * @param priority the priority
     */
    explicit Notification(const Application &application, const Alert &alert, const QString &title, const QString &text, const Icon &icon, int timeout = defaultTimeout(), Notification::Prioritys priority = NORMAL);

    /**
     * Creates and update Notification replacing an existing Notification
     * @param old the notification to replace
     * @param title the new title
     * @param text the new text body
     * @param icon the icon
     * @param timeout the timeout
     * @param priority the piority
     */
    explicit Notification(const Notification &old, const QString &title, const QString &text, const Icon &icon, int timeout = defaultTimeout(), Snore::Notification::Prioritys priority = NORMAL);

    /**
     * The copy constructor
     * @param other
     */
    Notification(const Notification &other);

    /**
     * The copy operator
     * @param other
     */
    Notification &operator=(const Notification &other);
    ~Notification();

    /**
     *
     * @return the internal id
     */
    uint id() const;

    /**
     * The timeout in seconds.
     * A timeout of 0 means the notification isSticky and will stay visible until dismissed by the user, if supported by the backend.
     * @see isSticky
     */
    const int &timeout() const;

    /**
     *
     * @return a valid Action if one was invoked otherwise an invalid.
     */
    const Action &actionInvoked() const;

    /**
     *
     * @return the associated application
     */
    const Application &application() const;

    /**
     * Returns the title of the notification.
     * @param flags the supported markup flags.
     */
    QString title(Utils::MARKUP_FLAGS flags = Utils::NO_MARKUP) const;

    /**
     * Returns the notification text.
     * @param flags the supported markup flags.
     */
    QString text(Utils::MARKUP_FLAGS flags = Utils::NO_MARKUP) const;

    /**
     *
     * @return the icon
     */
    const Icon &icon() const;

    /**
     *
     * @return the associated alert
     */
    const Alert &alert() const;

    /**
     * A sticki notification will stay visible until dismissed, if supported by the backend.
     * @return true if the timeout is 0
     */
    bool isSticky() const;

    /**
     * Some backends support priorities to indicate the urgency of the Notification
     * @return the priority
     */
    Notification::Prioritys priority() const;

    /**
     *
     * @return the availible actions
     * @see addAction
     */
    const QHash<int, Action> &actions() const;

    /**
     * Adds an Action to the Notification
     * @param a the action
     * @see actions
     */
    void addAction(const Action &a);

    /**
     *
     * @return the close reason
     */
    const Notification::CloseReasons &closeReason();

    /**
     * Returns notification specific hints.
     * A notification inherits the hints of its application.
     * @see Application::hints()
     */
    Hint &hints();

    /**
     *
     * @return hints associated with this notification
     */
    const Hint &constHints() const;

    /**
     *
     * @return whether this is a valid Notification
     */
    bool isValid() const;

    /**
     *
     * @return the old notification to be replaced
     * @see isUpdate
     */
    Notification old() const;

    /**
     *
     * @return whether this is an update to replace an old notification
     * @see old
     */
    bool isUpdate() const;

    /**
     *
     * @return a data pointer for internal use
     */
    NotificationData *data();

    /**
     *
     * @return the default timeout
     */
    static int defaultTimeout();

    //TODO: find a better name.
    void addActiveIn(const QObject *o);
    bool isActiveIn(const QObject *o) const;
    bool removeActiveIn(const QObject *o);

private:
    QExplicitlySharedDataPointer<NotificationData> d;

};

}
Q_DECLARE_METATYPE(Snore::Notification)

QDataStream &operator<< (QDataStream &stream, const Snore::Notification &noti);

SNORE_EXPORT QDebug operator<< (QDebug, const Snore::Notification::CloseReasons &);

SNORE_EXPORT QDebug operator<< (QDebug, const Snore::Notification::Prioritys &);

inline QDebug operator<< (QDebug debug, const Snore::Notification &noti)
{
    if (noti.isValid()) {
        debug.nospace() << "Snore::Notification(" << noti.title() << ", " << noti.text() << ", id = " << noti.id();
        if (noti.isUpdate()) {
            debug << ", oldID = " << noti.old().id();
        }
        debug << ")" ;
    } else {
        debug.nospace() << "Snore::Notification(0x00)" ;
    }
    return debug.maybeSpace();
}

#endif // NOTIFICATION_H
