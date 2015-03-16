/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2014  Patrick von Reth <vonreth@kde.org>

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

#ifndef NOTIFICATIONACTION_H
#define NOTIFICATIONACTION_H

#include "libsnore/snore_exports.h"

#include <QDataStream>

namespace  Snore
{

/**
 * Action contains informations about possible interactions with the notification system.
 * Some notification systems don't support actions but will report one if the notification was clicked,
 * in this case an invalid Action will be emitted.
 * @see isValid
 * @author Patrick von Reth \<vonreth at kde.org\>
 */

class SNORE_EXPORT Action
{
public:
    Action();

    /**
     * Creates an Action
     * @param id can be used to identify the action
     * @param name will be displayed in the notification system.
     */
    Action(int id, QString name);

    /**
     *
     * @return the id
     */
    int id() const;

    /**
     *
     * @return the name
     */
    QString name() const;

    /**
     *
     * @return whether this is a valid Action
     */
    bool isValid() const;

private:
    int m_id;
    QString m_name;
};
}

QDataStream &operator<< (QDataStream &stream, const Snore::Action &action);
#endif // NOTIFICATIONACTION_H
