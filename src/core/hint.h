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
#ifndef HINT_H
#define HINT_H

#include <QObject>
#include <QVariantHash>
#include <QDebug>

#include "snore_exports.h"

namespace Snore
{


class SNORE_EXPORT Hint
{
public:
    Hint();
    Hint(const Hint &other);

    void setValue(const QString &key, const QVariant &value);
    QVariant value(const QString & key, const QVariant & defaultValue = QVariant() ) const;
    bool contains ( const QString & key ) const;

    void setPrivateValue(const void *owner, const QString &key, const QVariant &value);
    QVariant privateValue(const void *owner, const QString & key, const QVariant & defaultValue = QVariant() ) const;
    bool containsPrivateValue(const void *owner, const QString & key ) const;
    
private:
    QVariantHash m_data;
    QVariantHash m_privateData;
    
};

}




#endif // HINT_H
