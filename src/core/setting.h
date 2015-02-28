/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2015  Patrick von Reth <vonreth@kde.org>

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

#ifndef SETTING_H
#define SETTING_H

#include "snore_exports.h"

#include <QVariant>

namespace Snore{
    class Setting;
}

SNORE_EXPORT QDataStream &operator<<(QDataStream &out, const Snore::Setting &myObj);
SNORE_EXPORT QDataStream &operator>>(QDataStream &in, Snore::Setting &myObj);

namespace Snore{

class SNORE_EXPORT Setting
{
public:
    Setting();
    Setting(const QVariant &value, bool specific = false);

   ~Setting();

    bool isSpecific() const;

    QVariant value() const;

    operator QVariant();

private:
    QVariant m_value;
    bool m_specific;

    friend SNORE_EXPORT QDataStream &(::operator<<)(QDataStream &out, const Snore::Setting &myObj);
    friend SNORE_EXPORT QDataStream &(::operator>>)(QDataStream &in, Snore::Setting &myObj);
};
}

Q_DECLARE_METATYPE(Snore::Setting)

#endif // SETTING_H
