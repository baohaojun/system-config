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
#include "setting.h"

using namespace Snore;

Setting::Setting()
{

}

Setting::Setting(const QVariant &value, bool specific):
    m_value(value),m_specific(specific)
{

}

Setting::~Setting()
{

}

bool Setting::isSpecific() const
{
    return m_specific;
}

QVariant Setting::value() const
{
    return m_value;
}

Snore::Setting::operator QVariant()
{
    return QVariant::fromValue(*this);
}


QDataStream &operator<<(QDataStream &out, const Snore::Setting &myObj)
{
    out << myObj.value() << myObj.isSpecific();
    return out;
}

QDataStream &operator>>(QDataStream &in, Snore::Setting &myObj)
{
    in >> myObj.m_value;
    in >> myObj.m_specific;
    return in;
}



