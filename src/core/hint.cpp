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

void Hint::setValue(const QString &key, QObject *value)
{
    m_data.insert(key.toLower(), qVariantFromValue(value));
    value->setProperty("hint_key",key.toLower());
    connect(value, SIGNAL(destroyed()), this, SLOT(slotValueDestroyed()),Qt::DirectConnection );
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
    QPair<quintptr,QString> pk((quintptr)owner,key.toLower());
    m_privateData.insert(pk, value);
}

void Hint::setPrivateValue(const void *owner, const QString &key, QObject *value)
{
    QPair<quintptr,QString> pk((quintptr)owner,key.toLower());
    m_privateData.insert(pk, qVariantFromValue(value));
    value->setProperty("hint_key",key.toLower());
    value->setProperty("hint_owner",(quintptr)owner);
    connect(value, SIGNAL(destroyed()), this, SLOT(slotValueDestroyed()), Qt::DirectConnection);
}


QVariant Hint::privateValue(const void *owner, const QString &k, const QVariant &defaultValue) const
{
    QPair<quintptr,QString> key((quintptr)owner, k.toLower());
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
    QPair<quintptr,QString> pk((quintptr)owner,key.toLower());
    return m_privateData.contains(pk);
}

void Hint::slotValueDestroyed()
{
    QObject * o = sender();
    QString key = o->property("hint_key").toString();
    if(!o->property("hint_owner").isNull())
    {
        QPair<quintptr,QString> pk(o->property("hint_owner").value<quintptr>(), key);
        m_privateData.remove(pk);
    }
    else
    {
        m_data.remove(key);
    }
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
    for(QHash< QPair<quintptr, QString>, QVariant>::const_iterator it = hint.m_privateData.constBegin();it != hint.m_privateData.constEnd();++it)
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
