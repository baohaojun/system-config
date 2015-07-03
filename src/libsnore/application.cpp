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
#include "application_p.h"

using namespace Snore;

Application::Application():
    d(NULL)
{}

Application::Application(const QString &name, const Icon &icon) :
    d(new ApplicationData(name, icon))

{
    addAlert(Alert(QObject::tr("Default"), icon));
}

Application::Application(const Application &other):
    d(other.d)
{

}

Application &Application::operator=(const Application &other)
{
    d = other.d;
    return *this;
}

Application::~Application()
{
}

void Application::addAlert(const Alert &alert)
{
    d->m_alerts.insert(alert.name().toUtf8(), alert);
}

QString Application::name() const
{
    return d->m_name;
}

const Icon &Application::icon()const
{
    return d->m_icon;
}

const QHash<QByteArray, Alert> &Application::alerts() const
{
    return d->m_alerts;
}

const Alert Application::defaultAlert() const
{
    return d->m_alerts.value("Default");
}

bool Application::isValid() const
{
    return d;
}

Hint &Application::hints()
{
    return d->m_hint;
}

const Hint &Application::constHints() const
{
    return d->m_hint;
}

QDebug operator<< (QDebug debug, const Snore::Application &app)
{
    if (app.isValid()) {
        debug << "Snore::Application(" << app.name() << ", ";
        foreach (const Alert &a, app.alerts()) {
            debug << a << ", ";
        }
        debug << ")" ;
    } else {
        debug << "Snore::Application(0x00)" ;
    }
    return debug.maybeSpace();
}
