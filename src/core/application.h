/****************************************************************************************
 * Copyright (c) 2010 Patrick von Reth <patrick.vonreth@gmail.com>                      *
 *                                                                                      *
 * This program is free software; you can redistribute it and/or modify it under        *
 * the terms of the GNU General Public License as published by the Free Software        *
 * Foundation; either version 2 of the License, or (at your option) any later           *
 * version.                                                                             *
 *                                                                                      *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY      *
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A      *
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.             *
 *                                                                                      *
 * You should have received a copy of the GNU General Public License along with         *
 * this program.  If not, see <http://www.gnu.org/licenses/>.                           *
 ****************************************************************************************/

#ifndef APPLICATION_H
#define APPLICATION_H
#include "snore_exports.h"
#include "notification/icon.h"

#include <QHash>

typedef QHash<QString,class Application*> ApplicationsList ;
typedef QHash<QString,class Alert*> AlertList;

class SNORE_EXPORT Application:public QObject
{
    Q_OBJECT
public:
    Application ( const QString &name, const SnoreIcon &icon = SnoreIcon(":/root/zzz.png"));
    Application();
    ~Application();
    void addAlert ( Alert *alert );
    const QString &name() const;
    const SnoreIcon &icon() const;
    const AlertList &alerts() const;
    bool isInitialized();
    void setInitialized ( bool b );


private:
    QString _name;
    SnoreIcon _icon;
    AlertList _alerts;
    bool _initialized;

};

class SNORE_EXPORT Alert:public QObject
{
    Q_OBJECT
public:
    Alert ( const QString &name,const QString &title="",const SnoreIcon &icon = SnoreIcon(":/root/zzz.png"),bool active=true );
    Alert();

    const QString &name() const;
    const QString &title() const;
    const SnoreIcon &icon() const;
    bool isActive() const;
private:
    QString _name;
    QString _title;
    SnoreIcon _icon;
    bool _active;
};


#endif // APPLICATION_H
