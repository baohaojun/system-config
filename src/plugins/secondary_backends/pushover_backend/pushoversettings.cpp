/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2015  Hannah von Reth <vonreth@kde.org>

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

PushoverSettings::PushoverSettings(Snore::SnorePlugin* plugin, QWidget* parent) :
    Snore::PluginSettingsWidget(plugin, parent),
    m_keyLineEdit(new QLineEdit(this)),
    m_soundLineEdit(new QLineEdit(this)),
    m_deviceLineEdit(new QLineEdit(this))
{
    addRow(tr("User Key:"), m_keyLineEdit, tr("The user key which can be found on your account page at <a href=\"https://pushover.net\">Pushover.net</a>."));
    addRow(tr("Sound:"), m_soundLineEdit, tr("The pushover sound that is played when a notification is received."));
    addRow(tr("Devices:"), m_deviceLineEdit, tr("The devices that are to receive the notifications."));
    addRow(QString(), new QLabel(tr("If you don't have an account yet please register at <a href=\"https://pushover.net\">Pushover.net</a>"), this));
}

PushoverSettings::~PushoverSettings()
{
}

void PushoverSettings::load()
{
    m_keyLineEdit->setText(settingsValue(QStringLiteral("UserKey")).toString());
    m_soundLineEdit->setText(settingsValue(QStringLiteral("Sound"), Snore::LocalSetting).toString());
    m_deviceLineEdit->setText(settingsValue(QStringLiteral("Devices"), Snore::LocalSetting).toString());
}

void PushoverSettings::save()
{
    setSettingsValue(QStringLiteral("UserKey"), m_keyLineEdit->text());
    setSettingsValue(QStringLiteral("Sound"), m_soundLineEdit->text(), Snore::LocalSetting);
    setSettingsValue(QStringLiteral("Devices"), m_deviceLineEdit->text(), Snore::LocalSetting);
}
