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

void Hint::setValue(const QString &key, const QVariant &value)
{
    m_data.insert(key.toLower(), value);
}

QVariant Hint::value(const QString &k, const QVariant &defaultValue) const
{
    QString key(k.toLower());
    if(m_data.contains(key))
    {
        return m_data.value(key);
    }
    else
    {
        return defaultValue;
    }
}

bool Hint::contains(const QString &key) const
{
    return m_data.contains(key.toLower());
}

void Hint::setPrivateValue(const void *owner, const QString &key, const QVariant &value)
{
    m_privateData.insert(QPair<const void*,QString>(owner,key.toLower()), value);
}


QVariant Hint::privateValue(const void *owner, const QString &k, const QVariant &defaultValue) const
{
    QPair<const void*,QString> key(owner,k.toLower());
    if(m_privateData.contains(key))
    {
        return m_privateData.value(key);
    }
    else
    {
        return defaultValue;
    }
}


bool Hint::containsPrivateValue(const void *owner, const QString &key) const
{
    return m_privateData.contains(QPair<const void*,QString>(owner,key.toLower()));
}

QDebug operator<<( QDebug debug, const Snore::Hint &hint )
{
    debug << "Snore::Hint(";
    for(QVariantHash::const_iterator it = hint.m_data.constBegin();it != hint.m_data.constEnd();++it)
    {
        if(it != hint.m_data.constBegin())
        {
            debug << ", ";
        }
        debug << "(" << it.key() << ", " << it.value();
    }
    for(QHash< QPair<const void*, QString>, QVariant>::const_iterator it = hint.m_privateData.constBegin();it != hint.m_privateData.constEnd();++it)
    {
        if(it != hint.m_privateData.constBegin())
        {
            debug << ", ";
        }
        debug << "(" << it.key() << ", " << it.value();
    }
    debug << ")" ;
    return debug.maybeSpace();
}
