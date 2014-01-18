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
#include "hint.h"

using namespace Snore;

Hint::Hint()
{
}

Hint::Hint(const Hint &other):
    m_data(other.m_data)
{
}

void Hint::setValue(const QString &key, const QVariant &value)
{
    m_data[key] = value;
}

QVariant Hint::value(const QString &key, const QVariant &defaultValue) const
{
    if(m_data.contains(key))
    {
        return m_data[key];
    }
    else
    {
        return defaultValue;
    }
}

bool Hint::contains(const QString &key) const
{
    return m_data.contains(key);
}

//QDebug operator<<( QDebug debug, const Snore::Hint &hint )
//{
//    debug << "Snore::Hint(" << hint.m_data << ")" ;
//    return debug.maybeSpace();
//}


void Hint::setPrivateValue(const void *owner, const QString &key, const QVariant &value)
{
    m_privateData[QString("%1-%2").arg(QString::number((quintptr)owner),key)] = value;
}


QVariant Hint::privateValue(const void *owner, const QString &k, const QVariant &defaultValue) const
{
    QString key(QString("%1-%2").arg(QString::number((quintptr)owner),k));
    if(m_privateData.contains(key))
    {
        return m_privateData[key];
    }
    else
    {
        return defaultValue;
    }
}


bool Hint::containsPrivateValue(const void *owner, const QString &key) const
{
    return m_privateData.contains(QString("%1-%2").arg(QString::number((quintptr)owner),key));
}
