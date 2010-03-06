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
#include <QString>
#include <QStringList>
#include <QDebug>
#include <QHash>
#include <QSharedPointer>

typedef QHash<QString,QSharedPointer<class Application> > ApplicationsList ;
typedef QHash<QString,QSharedPointer<class Alert> > AlertList;
class SNORE_EXPORT Application
{
public:
    Application(const QString &name);
    Application();
    AlertList alerts;
    QString name;


    void addAlert(const QString &alert,const QString &title);
};

class SNORE_EXPORT Alert{
public:
    Alert(const QString &name,const QString &title);
    Alert(const QString &name,const QString &title,bool active);
    Alert();
    QString name;
    QString title;
    bool active;
};


#endif // APPLICATION_H
