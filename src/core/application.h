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
#include "alert.h"

#include <QHash>
namespace Snore{

class ApplicationData;

class SNORE_EXPORT Application
{
public:
    Application();
    Application ( const QString &name, const Icon &icon = Icon(":/root/snore.png"));
    Application(const Application &other);
    Application &operator=(const Application &other);
    ~Application();

    void addAlert(const  Alert &alert);
    QString name() const;
    const Icon &icon() const;
    const QHash<QString,Alert> &alerts() const;
    bool isValid() const;

    Hint &hints();
    const Hint &hints() const;
private:
     QExplicitlySharedDataPointer<ApplicationData> d;


};


inline QDebug operator<< ( QDebug debug, const Snore::Application &app )
{
    if(app.isValid())
    {
        debug << "Snore::Application(" << app.name() << ", " << app.alerts() << ")";//," << app.hints() << ")" ;
    }
    else
    {
        debug << "Snore::Application(0x00)" ;
    }
    return debug.maybeSpace();
}

}

#endif // APPLICATION_H
