/****************************************************************************************
 * Copyright (c) 2010-2012 Patrick von Reth <patrick.vonreth@gmail.com>                 *
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
namespace Snore{


Application::Application (const QString &name, const SnoreIcon &icon) :
        m_name ( name ),
        m_icon(icon),
        m_initialized ( false )
{
}

Application::Application() :
        m_name ( "Error: Uninitialized Application" )
{}

Application::~Application()
{
    foreach ( Alert *a,m_alerts )
    {
        a->deleteLater();
    }
}

void Application::addAlert ( Alert *alert )
{
    m_alerts.insert ( alert->name(),alert );
}

const QString &Application::name() const
{
    return m_name;
}

const SnoreIcon &Application::icon()const
{
    return m_icon;
}

const AlertList &Application::alerts() const
{
    return m_alerts;
}

bool Application::isInitialized()
{
    return m_initialized;
}

void Application::setInitialized ( bool b )
{
    m_initialized = b;
}

Alert::Alert (const QString &name, const QString &title, const SnoreIcon &icon, bool active) :
        m_name ( name ),
        m_title ( title ),
        m_icon(icon),
        m_active ( active )
{}

Alert::Alert() :
        m_active ( false )
{}


const QString &Alert::name() const
{
    return m_name;
}

const QString &Alert::title() const
{
    return m_title;
}

const SnoreIcon &Alert::icon() const
{
    return m_icon;
}

bool Alert::isActive() const
{
    return m_active;
}

}
