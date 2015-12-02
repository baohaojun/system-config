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
#include "nmasettings.h"

#include "libsnore/plugins/plugins.h"

#include <QLabel>
#include <QLineEdit>

NotifyMyAndroidSettings::NotifyMyAndroidSettings(Snore::SnorePlugin *plugin, QWidget *parent) :
    Snore::PluginSettingsWidget(plugin, parent),
    m_lineEdit(new QLineEdit)
{
    addRow(tr("Api Key:"), m_lineEdit, tr("The api key which can be found in your account page at <a href=\"http://notifymyandroid.com/\">Notifymyandroid.com</a>."));
    addRow(QString(), new QLabel(tr("If you don't have an account yet please register at <a href=\"http://notifymyandroid.com/\">Notifymyandroid.com</a>."), this));
}

NotifyMyAndroidSettings::~NotifyMyAndroidSettings()
{
}

void NotifyMyAndroidSettings::load()
{
    m_lineEdit->setText(settingsValue(QStringLiteral("ApiKey")).toString());
}

void NotifyMyAndroidSettings::save()
{
    setSettingsValue(QStringLiteral("ApiKey"), m_lineEdit->text());
}
