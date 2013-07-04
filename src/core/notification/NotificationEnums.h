/****************************************************************************************
 * Copyright (c) 2013 Patrick von Reth <vonreth@kde.org>                                *
 *                                                                                      *
 * This program is free software; you can redistribute it and/or modify it under        *
 * the terms of the GNU General Public License as published by the Free Software        *
 * Foundation; either version 2 of the License, or (at your option) any later           *
 * version.                                                                             *
 *                                                                                      *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY      *
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A      *
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.             *
 *                                                                                      *
 * You should have received a copy of the GNU General Public License along with         *
 * this program.  If not, see <http://www.gnu.org/licenses/>.                           *
 ****************************************************************************************/


#ifndef NOTIFICATIONENUMS_H
#define NOTIFICATIONENUMS_H

#include <QFlags>

namespace Snore{
namespace NotificationEnums{

namespace Prioritys{
enum priority{
    LOW = -1,
    NORMAL = 0,
    HIGH = +1
};
Q_DECLARE_FLAGS(prioritys, priority)
Q_DECLARE_OPERATORS_FOR_FLAGS(prioritys)
}
namespace CloseReasons{
enum closeReason
{
    NONE,
    TIMED_OUT,
    DISMISSED,
    CLOSED
};
Q_DECLARE_FLAGS(closeReasons, closeReason)
Q_DECLARE_OPERATORS_FOR_FLAGS(closeReasons)
}
}
}
#endif // NOTIFICATIONENUMS_H
