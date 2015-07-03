/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2015  Patrick von Reth <vonreth@kde.org>

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
#include "nmasettings.h"

#include "plugins/plugins.h"

#include <QLineEdit>

NotifyMyAndroidSettings::NotifyMyAndroidSettings(Snore::SnorePlugin *plugin, QWidget *parent) :
    Snore::PluginSettingsWidget(plugin, parent),
    m_lineEdit(new QLineEdit)
{
    addRow(tr("Api Key:"), m_lineEdit);
}

NotifyMyAndroidSettings::~NotifyMyAndroidSettings()
{
}

void NotifyMyAndroidSettings::load()
{
    m_lineEdit->setText(settingsValue(QLatin1String("ApiKey")).toString());
}

void NotifyMyAndroidSettings::save()
{
    setSettingsValue(QLatin1String("ApiKey"), m_lineEdit->text());
}
