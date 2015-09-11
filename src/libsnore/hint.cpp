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
#include "lambdahint.h"
#include "log.h"

using namespace Snore;

Hint::Hint()
{
}

void Hint::setValue(const QByteArray &key, const QVariant &value)
{
    m_data.insert(key, value);
}

QVariant Hint::value(const QByteArray &key) const
{
    QVariant out = m_data.value(key);
    if (out.canConvert<LambdaHint>()) {
        return out.value<LambdaHint>()();
    }
    return out;
}

QVariant Hint::take(const QByteArray &key)
{
    QVariant out = m_data.take(key);
    if (out.canConvert<LambdaHint>()) {
        return out.value<LambdaHint>()();
    }
    return out;
}

bool Hint::contains(const QByteArray &key) const
{
    return m_data.contains(key);
}

void Hint::setPrivateValue(const void *owner, const QByteArray &key, const QVariant &value)
{
    m_privateData.insert(qMakePair<const quintptr, const QByteArray>((quintptr)owner, key), value);
}

QVariant Hint::privateValue(const void *owner, const QByteArray &key) const
{
    QVariant out = m_privateData.value(qMakePair<const quintptr, const QByteArray>((quintptr)owner, key));
    if (out.canConvert<LambdaHint>()) {
        return out.value<LambdaHint>()();
    }
    return out;
}

bool Hint::containsPrivateValue(const void *owner, const QByteArray &key) const
{
    return m_privateData.contains(qMakePair<const quintptr, const QByteArray>((quintptr)owner, key));
}

QVariant Hint::takePrivateValue(const void *owner, const QByteArray &key)
{
    QVariant out = m_privateData.take(qMakePair<const quintptr, const QByteArray>((quintptr)owner, key));
    if (out.canConvert<LambdaHint>()) {
        return out.value<LambdaHint>()();
    }
    return out;
}

QDebug operator<<(QDebug debug, const Snore::Hint &hint)
{
    debug << "Snore::Hint(";
    for (auto it = hint.m_data.cbegin(); it != hint.m_data.cend(); ++it) {
        debug << ", ";
        debug << "(" << it.key() << ", " << it.value();
    }
    for (auto it = hint.m_privateData.cbegin(); it != hint.m_privateData.cend(); ++it) {
        debug << ", ";
        debug << "(" << it.key() << ", " << it.value();
    }
    debug << ")" ;
    return debug.maybeSpace();
}
