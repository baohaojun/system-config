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

#include "application.h"


Application::Application(const QString &name):name(name)
{}

Application::Application():name("Error: Uninitialized Application")
{}

void Application::addAlert(const QString &alert,const QString &title)
{
    alerts.insert(alert,QSharedPointer<Alert>(new Alert(alert,title)));
}


Alert::Alert(const QString &name,const QString &title):
        name(name),
        title(title),
        active(true)
{}

Alert::Alert(const QString &name,const QString &title,bool active):
        name(name),
        title(title),
        active(active)
{}

Alert::Alert():
        active(false)
{}



