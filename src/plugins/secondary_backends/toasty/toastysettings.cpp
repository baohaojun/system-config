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
#include "toastysettings.h"

#include "plugins/plugins.h"

#include <QLabel>
#include <QLineEdit>

ToastySettings::ToastySettings(Snore::SnorePlugin *plugin, QWidget *parent) :
    Snore::PluginSettingsWidget(plugin, parent),
    m_lineEdit(new QLineEdit)
{
    addRow(tr("Device ID:"), m_lineEdit, tr("The device id which can be found in the toasty app."));
    addRow(QString(), new QLabel(tr("<a href=\"http://supertoasty.com/\">Supertoasty.com</a>"), this));
}

ToastySettings::~ToastySettings()
{
}

void ToastySettings::load()
{
    m_lineEdit->setText(settingsValue(QLatin1String("DeviceID")).toString());
}

void ToastySettings::save()
{
    setSettingsValue(QLatin1String("DeviceID"), m_lineEdit->text());
}
