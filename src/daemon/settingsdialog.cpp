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
#include "settingsdialog.h"
#include "ui_settingsdialog.h"
#include "snore.h"

#include <QTabWidget>
#include <QFormLayout>
#include <QCheckBox>

using namespace Snore;

SettingsDialog::SettingsDialog(SnoreCore *snore, QWidget *parent) :
    QDialog(parent),
    ui(new Ui::SettingsDialog),
    m_snore(snore)
{
    ui->setupUi(this);
    for(auto widget : snore->settingWidgets())
    {
        ui->tabWidget->addTab(widget,widget->name());
        widget->loadSettings();
        m_tabs.append(widget);
    }
}

SettingsDialog::~SettingsDialog()
{
    delete ui;
}

void SettingsDialog::on_buttonBox_accepted()
{
    for( auto w:m_tabs)
    {
        w->saveSettings();
    }
}
