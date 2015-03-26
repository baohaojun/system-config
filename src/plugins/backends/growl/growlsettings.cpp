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
#include "growlsettings.h"

#include <QLineEdit>

using namespace Snore;

GrowlSettings::GrowlSettings(SnorePlugin *plugin, QWidget *parent):
    PluginSettingsWidget(plugin, parent),
    m_host(new QLineEdit),
    m_password(new QLineEdit)
{
    m_password->setEchoMode(QLineEdit::Password);
    addRow(tr("Host:"), m_host);
    addRow(tr("Password:"), m_password);
}

GrowlSettings::~GrowlSettings()
{

}

void GrowlSettings::load()
{
    m_host->setText(value("Host").toString());
    m_password->setText(value("Password").toString());
}

void GrowlSettings::save()
{
    setValue("Host", m_host->text());
    setValue("Password", m_password->text());
}

