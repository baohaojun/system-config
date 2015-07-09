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

#ifndef ALERT_P_H
#define ALERT_P_H

#include <QString>
#include <QSharedData>

#include "alert.h"

namespace Snore
{

class AlertData : public QSharedData
{
    friend class Alert;
public:
    AlertData(const QString &name, const Icon &icon);
    ~AlertData();

    QString m_name;
    Icon m_icon;

private:
    Q_DISABLE_COPY(AlertData)

};
}

#endif // ALERT_P_H
