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
#include "pushoversettings.h"

#include "plugins/plugins.h"

#include <QLineEdit>

PushoverSettings::PushoverSettings(Snore::SnorePlugin *plugin, QWidget *parent) :
    Snore::PluginSettingsWidget(plugin, parent),
    m_keyLineEdit(new QLineEdit(this)),
    m_soundLineEdit(new QLineEdit(this)),
    m_deviceLineEdit(new QLineEdit(this))
{
    addRow(tr("User Key:"), m_keyLineEdit);
    addRow(tr("Sound:"), m_soundLineEdit);
    addRow(tr("Devices:"), m_deviceLineEdit);
}

PushoverSettings::~PushoverSettings()
{
}

void PushoverSettings::load()
{
    m_keyLineEdit->setText(settingsValue(QLatin1String("UserKey")).toString());
    m_soundLineEdit->setText(settingsValue(QLatin1String("Sound"), Snore::LOCAL_SETTING).toString());
    m_deviceLineEdit->setText(settingsValue(QLatin1String("Devices"), Snore::LOCAL_SETTING).toString());
}

void PushoverSettings::save()
{
    setSettingsValue(QLatin1String("UserKey"), m_keyLineEdit->text());
    setSettingsValue(QLatin1String("Sound"), m_soundLineEdit->text(), Snore::LOCAL_SETTING);
    setSettingsValue(QLatin1String("Devices"), m_deviceLineEdit->text(), Snore::LOCAL_SETTING);
}
