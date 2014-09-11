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

#include "notificationaction.h"

using namespace Snore;

Action::Action():
    m_id(-1)
{

}

Action::Action(int id, QString name):
    m_id(id),
    m_name(name)
{

}

QString Action::name() const
{
    return m_name;
}

bool Action::isValid() const
{
    return !m_name.isNull();
}

int Action::id() const
{
    return m_id;
}

QDataStream &operator<< (QDataStream &stream, const Action &a)
{
    stream << a.id() << a.name();
    return stream;
}
