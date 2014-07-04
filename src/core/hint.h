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
class Hint;
}

SNORE_EXPORT QDebug operator<< ( QDebug, const Snore::Hint &);

namespace Snore
{
/**
 * Hint contains extra information accesible by key.
 * The keys are case insensitive.
 */

class SNORE_EXPORT Hint : public QObject
{
    Q_OBJECT
public:
    Hint();

    /**
     * Sets the value for the key
     * @param key the key
     * @param value the value
     */
    void setValue(const QString &key, const QVariant &value);

    /**
     * Sets the value for the key
     * @param key the key
     * @param value the value
     */
    void setValue(const QString &key, QObject *value);

    /**
     * The associated value of the key if present, returns the default value otherwise.
     * @param key the key
     * @param defaultValue the fallback value
     */
    QVariant value(const QString & key, const QVariant & defaultValue = QVariant() ) const;

    /**
     *
     * @param key the key
     * @return whether the key is set
     */
    bool contains ( const QString & key ) const;

    /**
     * Sets the value for the key depending on the owner
     * @param owner the owner
     * @param key the key
     * @param value the value
     */
    void setPrivateValue(const void *owner, const QString &key, const QVariant &value) const;

    /**
     * Sets the value for the key depending on the owner
     * @param owner the owner
     * @param key the key
     * @param value the value
     */
    void setPrivateValue(const void *owner, const QString &key, QObject *value) const;

    /**
     * The associated value of the key if present, returns the default value otherwise.
     * @param owner the owner
     * @param key the key
     * @param defaultValue the fallback value
     */
    QVariant privateValue(const void *owner, const QString & key, const QVariant & defaultValue = QVariant() ) const;

    /**
     *
     * @param owner the owner
     * @param key the key
     * @return whether the key is set
     */
    bool containsPrivateValue(const void *owner, const QString & key ) const;

private slots:
    void slotValueDestroyed();

private:
    QVariantHash m_data;
    mutable QHash<QPair<quintptr,QString>, QVariant> m_privateData;

    friend SNORE_EXPORT QDebug (::operator<<) ( QDebug, const Snore::Hint &);

};


}




#endif // HINT_H
