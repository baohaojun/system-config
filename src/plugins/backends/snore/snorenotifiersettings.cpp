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
#include "snorenotifiersettings.h"
#include "snore.h"

#include <QComboBox>

SnoreSettings::SnoreSettings(Snore::SnorePlugin *snore, QWidget *parent) :
    PluginSettingsWidget(snore, parent),
    m_comboBox(new QComboBox)
{
    m_comboBox->addItem(tr("Top Left Corner"), Qt::TopLeftCorner);
    m_comboBox->addItem(tr("Top Right Corner"), Qt::TopRightCorner);
    m_comboBox->addItem(tr("Bottom Left Corner"), Qt::BottomLeftCorner);
    m_comboBox->addItem(tr("Bottom Right Corner"), Qt::BottomRightCorner);
    addRow(tr("Position:"), m_comboBox);
}

SnoreSettings::~SnoreSettings()
{
}

void SnoreSettings::load()
{
    m_comboBox->setCurrentIndex(settingsValue(QStringLiteral("Position")).toInt());
}

void SnoreSettings::save()
{
    setSettingsValue(QStringLiteral("Position"), m_comboBox->currentIndex());
}
