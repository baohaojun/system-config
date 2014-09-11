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

#include "alert.h"
#include "alert_p.h"

using namespace Snore;

Alert::Alert() :
    d(NULL)
{}

Alert::Alert(const QString &name, const Icon &icon, bool active):
    d(new AlertData(name, icon, active))
{}

Alert::Alert(const Alert &other):
    d(other.d)
{

}

Alert &Alert::operator=(const Alert &other)
{
    d = other.d;
    return *this;
}

Alert::~Alert()
{

}

QString Alert::name() const
{
    return d->m_name;
}

const Icon &Alert::icon() const
{
    return d->m_icon;
}

bool Alert::isActive() const
{
    return d->m_active;
}

bool Alert::isValid() const
{
    return d;
}

QDebug operator<<(QDebug debug, const Alert &alert)
{
    if (alert.isValid()) {
        debug << "Snore::Alert(" << alert.name() << ")" ;
    } else {
        debug << "Snore::Alert(0x00)" ;
    }
    return debug.maybeSpace();
}
