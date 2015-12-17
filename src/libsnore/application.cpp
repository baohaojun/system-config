/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2015  Hannah von Reth <vonreth@kde.org>

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
#include "application_p.h"
#include "snore_p.h"

using namespace Snore;

Application::Application():
    d(nullptr)
{}

Application::Application(const QString& name, const Icon& icon) :
    d(new ApplicationData(name, name, icon))
{
}

Application::Application(const QString& key, const QString& name, const Icon& icon) :
    d(new ApplicationData(key, name, icon))
{
}

Application::Application(const Application& other):
    d(other.d)
{
}

Application& Application::operator=(const Application& other)
{
    d = other.d;
    return *this;
}

Application::~Application()
{
}

void Application::addAlert(const Alert& alert)
{
    Q_ASSERT_X(!SnoreCore::instance().aplications().contains(key()), Q_FUNC_INFO,
               "Alerts must be added before the application is Registered.");
    d->m_alerts.insert(alert.key(), alert);
}

QString Application::key() const
{
    return d->m_key;
}

QString Application::name() const
{
    return d->m_name;
}

const Icon& Application::icon()const
{
    return d->m_icon;
}

const QHash<QString, Alert>& Application::alerts() const
{
    return d->m_alerts;
}

const Alert Application::defaultAlert() const
{
    return d->m_defaultAlert;
}

bool Application::isValid() const
{
    return d;
}

Hint& Application::hints()
{
    return d->m_hint;
}

const Hint& Application::constHints() const
{
    return  const_cast<Hint&>(const_cast<Application*>(this)->hints());
}

QDebug operator<< (QDebug debug, const Snore::Application& app)
{
    if (app.isValid()) {
        debug << "Snore::Application(" << app.name() << ", ";
        foreach (const Alert & a, app.alerts()) {
            debug << a << ", ";
        }
        debug << ")" ;
    } else {
        debug << "Snore::Application(0x00)" ;
    }
    return debug.maybeSpace();
}
