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

#include "../snore_exports.h"

#include <QDataStream>


namespace  Snore
{

class SNORE_EXPORT Action
{
public:
    Action();
    Action(int id,QString name);

    int id() const;
    QString name() const;
    bool isValid() const;

private:
    int m_id;
    QString m_name;
};
}

QDataStream &operator<< ( QDataStream & stream, const Snore::Action &action);
#endif // NOTIFICATIONACTION_H
