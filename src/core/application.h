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

#ifndef APPLICATION_H
#define APPLICATION_H
#include "snore_exports.h"
#include "hint.h"
#include "notification/icon.h"

#include <QHash>
namespace Snore{

class SNORE_EXPORT Alert
{
public:
    Alert();
    Alert ( const QString &name,const QString &title="",const Icon &icon = Icon(":/root/snore.png"),bool active=true );
    Alert(const Alert &other);

    QString name() const;
    QString title() const;
    const Icon &icon() const;
    bool isActive() const;
    bool isValid() const;
private:
    QString m_name;
    QString m_title;
    Icon m_icon;
    bool m_active;
};

class SNORE_EXPORT Application
{
public:
    Application();
    Application ( const QString &name, const Icon &icon = Icon(":/root/snore.png"));
    Application(const Application &other);
    ~Application();

    void addAlert(const  Alert &alert);
    QString name() const;
    const Icon &icon() const;
    const QHash<QString,Alert> &alerts() const;
    bool isValid() const;

    Hint &hints();
    const Hint &hints() const;
private:
    QString m_name;
    Icon m_icon;
    QHash<QString,Alert> m_alerts;
    Hint m_hint;

};



}

#endif // APPLICATION_H
