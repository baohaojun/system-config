/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2015  Patrick von Reth <vonreth@kde.org>

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
#include "ui_snorenotifiersettings.h"
#include "snore.h"

using namespace Snore;

SnoreNotifierSettings::SnoreNotifierSettings(SnorePlugin *snore, QWidget *parent) :
    PluginSettingsWidget(snore,parent),
    ui(new Ui::SnoreNotifierSettings)
{
    ui->setupUi(this);
    ui->comboBox->addItem("TopLeftCorner",Qt::TopLeftCorner);
    ui->comboBox->addItem("TopRightCorner",Qt::TopRightCorner);
    ui->comboBox->addItem("BottomLeftCorner",Qt::BottomLeftCorner);
    ui->comboBox->addItem("BottomRightCorner",Qt::BottomRightCorner);
}

SnoreNotifierSettings::~SnoreNotifierSettings()
{
    delete ui;
}

void SnoreNotifierSettings::load()
{
    ui->comboBox->setCurrentIndex(m_snorePlugin->value("Position").toInt());
}

void SnoreNotifierSettings::save()
{
    m_snorePlugin->setValue("Position", ui->comboBox->currentIndex());
}
