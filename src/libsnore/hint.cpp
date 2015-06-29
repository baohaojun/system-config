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
#include "log.h"

using namespace Snore;

Hint::Hint()
{
}

void Hint::setValue(const QString &key, const QVariant &value)
{
    m_data.insert(key.toLower(), value);
}

QVariant Hint::value(const QString &k) const
{
    return m_data.value(k.toLower());
}

QVariant Hint::take(const QString &key)
{
    return m_data.take(key.toLower());
}

bool Hint::contains(const QString &key) const
{
    return m_data.contains(key.toLower());
}

void Hint::setPrivateValue(const void *owner, const QString &key, const QVariant &value) const
{
    QPair<quintptr, QString> pk((quintptr)owner, key.toLower());
    m_privateData.insert(pk, value);
}

QVariant Hint::privateValue(const void *owner, const QString &k) const
{
    QPair<quintptr, QString> key((quintptr)owner, k.toLower());
    return m_privateData.value(key);
}

bool Hint::containsPrivateValue(const void *owner, const QString &key) const
{
    QPair<quintptr, QString> pk((quintptr)owner, key.toLower());
    return m_privateData.contains(pk);
}

QVariant Hint::takePrivateValue(const void *owner, const QString &key)
{
    QPair<quintptr, QString> pk((quintptr)owner, key.toLower());
    return m_privateData.take(pk);
}

QDebug operator<<(QDebug debug, const Snore::Hint &hint)
{
    debug << "Snore::Hint(";
    for (QVariantHash::const_iterator it = hint.m_data.constBegin(); it != hint.m_data.constEnd(); ++it) {
        if (it != hint.m_data.constBegin()) {
            debug << ", ";
        }
        debug << "(" << it.key() << ", " << it.value();
    }
    for (QHash< QPair<quintptr, QString>, QVariant>::const_iterator it = hint.m_privateData.constBegin(); it != hint.m_privateData.constEnd(); ++it) {
        if (it != hint.m_privateData.constBegin()) {
            debug << ", ";
        }
        debug << "(" << it.key() << ", " << it.value();
    }
    debug << ")" ;
    return debug.maybeSpace();
}
