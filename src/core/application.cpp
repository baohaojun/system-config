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

#include "application.h"


using namespace Snore;

Application::Application (const QString &name, const Icon &icon) :
    m_name(name),
    m_icon(icon)
{
}

Application::Application(const Application &other):
    m_name(other.m_name),
    m_icon(other.m_icon),
    m_alerts(other.m_alerts)
{

}

Application::Application()
{}

Application::~Application()
{
}

void Application::addAlert(const Alert &alert)
{
    m_alerts.insert(alert.name(), alert);
}

QString Application::name() const
{
    return m_name;
}

const Icon &Application::icon()const
{
    return m_icon;
}

const QHash<QString, Alert> &Application::alerts() const
{
    return m_alerts;
}

bool Application::isValid() const
{
    return m_name.isNull();
}

Hint &Application::hints()
{
    return m_hint;
}

Alert::Alert (const QString &name, const QString &title, const Icon &icon, bool active):
    m_name(name),
    m_title(title),
    m_icon(icon),
    m_active(active)
{}

Alert::Alert(const Alert &other):
    m_name(other.m_name),
    m_title(other.m_title),
    m_icon(other.m_icon),
    m_active(other.m_active)
{

}

Alert::Alert() :
    m_active ( false )
{}


QString Alert::name() const
{
    return m_name;
}

QString Alert::title() const
{
    return m_title;
}

const Icon &Alert::icon() const
{
    return m_icon;
}

bool Alert::isActive() const
{
    return m_active;
}

bool Alert::isValid() const
{
    return m_name.isNull();
}
