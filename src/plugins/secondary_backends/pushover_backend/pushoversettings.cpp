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

#include <QLabel>
#include <QLineEdit>

PushoverSettings::PushoverSettings(Snore::SnorePlugin *plugin, QWidget *parent) :
    Snore::PluginSettingsWidget(plugin, parent),
    m_keyLineEdit(new QLineEdit(this)),
    m_soundLineEdit(new QLineEdit(this)),
    m_deviceLineEdit(new QLineEdit(this))
{
    addRow(tr("User Key:"), m_keyLineEdit, tr("The user key which can be found on your account page at <a href=\"https://pushover.net\">Pushover.net</a>."));
    addRow(tr("Sound:"), m_soundLineEdit, tr("The pushover sound that it played when a notification is recieved."));
    addRow(tr("Devices:"), m_deviceLineEdit, tr("The devices that are to recieve the notifications."));
    addRow(QString(), new QLabel(tr("If you don't have an accout yet please register at <a href=\"https://pushover.net\">Pushover.net</a>"), this));
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
