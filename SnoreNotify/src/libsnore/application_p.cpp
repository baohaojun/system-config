/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2014-2015  Hannah von Reth <vonreth@kde.org>

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

#include "application_p.h"
#include "lambdahint.h"
#include "snore_p.h"

#include <QGuiApplication>

using namespace Snore;

ApplicationData::ApplicationData(const QString &key, const QString &name, const Icon &icon):
    m_key(key),
    m_name(name),
    m_icon(icon),
    m_defaultAlert(qApp->translate("Default Alert", "Default"), icon)
{
    Q_ASSERT_X(!name.isEmpty(), Q_FUNC_INFO, "invalid name detected");
    m_alerts.insert(m_defaultAlert.key(), m_defaultAlert);

    m_hint.setValue("pushover-token", QLatin1String("aFB1TPCyZkkr7mubCGEKy5vJEWak9t"));
    m_hint.setValue("use-markup", false);
    m_hint.setValue("silent", QVariant::fromValue(LambdaHint([]() {
        return SnoreCore::instance().settingsValue(QStringLiteral("Silent"), LocalSetting);
    })));

}

ApplicationData::~ApplicationData()
{

}
