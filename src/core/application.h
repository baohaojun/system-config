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
#include "notification/icon.h"

#include <QHash>
namespace Snore{
class Application;
class Alert;

typedef QHash<QString,Application*> ApplicationsList ;
typedef QHash<QString,Alert*> AlertList;

class SNORE_EXPORT Application : public QObject
{
    Q_OBJECT
public:
    Application ( const QString &name, const Icon &icon = Icon(":/root/snore.png"));
    Application();
    ~Application();
    void addAlert ( Alert *alert );
    const QString &name() const;
    const Icon &icon() const;
    const AlertList &alerts() const;


private:
    QString m_name;
    Icon m_icon;
    AlertList m_alerts;

};

class SNORE_EXPORT Alert:public QObject
{
    Q_OBJECT
public:
    Alert ( const QString &name,const QString &title="",const Icon &icon = Icon(":/root/snore.png"),bool active=true );
    Alert();

    const QString &name() const;
    const QString &title() const;
    const Icon &icon() const;
    bool isActive() const;
private:
    QString m_name;
    QString m_title;
    Icon m_icon;
    bool m_active;
};


}

#endif // APPLICATION_H
