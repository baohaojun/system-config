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

#ifndef ALERT_H
#define ALERT_H

#include "snore_exports.h"
#include "notification/icon.h"

#include <QSharedData>

namespace Snore{

class AlertData;

class SNORE_EXPORT Alert
{
    friend class AlertData;
public:
    Alert();
    Alert(const QString &name, const QString &title="", const Icon &icon = Icon(":/root/snore.png"), bool active=true );
    Alert(const Alert &other);
    Alert &operator=(const Alert &other);
    ~Alert();

    QString name() const;
    QString title() const;
    const Icon &icon() const;
    bool isActive() const;
    bool isValid() const;
private:
    QExplicitlySharedDataPointer<AlertData> d;

};
}

QDebug operator<< ( QDebug debug, const Snore::Alert &alert );


#endif // ALERT_H
