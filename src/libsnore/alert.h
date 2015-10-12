/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2014-2015  Hannah von Reth <vonreth@kde.org>

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

#ifndef ALERT_H
#define ALERT_H

#include "snore_exports.h"
#include "notification/icon.h"

#include <QSharedData>

namespace Snore
{

class AlertData;

/**
 *  Alert contains all relevant data to manage different alerts registred with the notification backend.
 *  Application uses a shared datamodel, its content is never copied and automatically released.
 *
 * @author Hannah von Reth \<vonreth at kde.org\>
 */

class SNORE_EXPORT Alert
{
public:
    Alert();

    /**
     * Creates an alert.
     * @param name the name of the Alert.
     * @param icon the Icon of the Alert.
     */
    explicit Alert(const QString &name, const Icon &icon);

    /**
     * Creates an alert.
     * @param name the key of the Alert used in Application::alerts().
     * @param name the name of the Alert.
     * @param icon the Icon of the Alert.
     */
    explicit Alert(const QString &key, const QString &name, const Icon &icon);

    /**
     * Creates a copy of other
     * @param other
     */
    Alert(const Alert &other);

    /**
     * Creates a copy of other.
     * @param other
     */
    Alert &operator=(const Alert &other);
    ~Alert();

    /**
     * Returns the key of the Alert, used in Application::alerts().
     * Might be identically to name().
     */
    QString key() const;

    /**
     * Returns the name of the Alert.
     */
    QString name() const;

    /**
     * Returns the icon of the Alert.
     */
    const Icon &icon() const;

    /**
     * Returns whether the Alert is valid.
     */
    bool isValid() const;

private:
    QExplicitlySharedDataPointer<AlertData> d;

};
}

QDebug SNORE_EXPORT operator<< (QDebug debug, const Snore::Alert &alert);

#endif // ALERT_H
