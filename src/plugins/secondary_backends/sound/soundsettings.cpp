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
#include "soundsettings.h"
#include "ui_soundsettings.h"
#include "plugins/plugins.h"

#include <QFileDialog>


using namespace Snore;

SoundSettings::SoundSettings(SnorePlugin *snorePlugin, QWidget *parent) :
    PluginSettingsWidget(snorePlugin,parent),
    ui(new Ui::SoundSettings)
{
    ui->setupUi(this);
}

SoundSettings::~SoundSettings()
{
    delete ui;
}

void SoundSettings::load()
{
    ui->soundFileLineEdit->setText(m_snorePlugin->value("Sound").toString());
}

void SoundSettings::save()
{
    m_snorePlugin->setValue("Sound",ui->soundFileLineEdit->text());
}

void SoundSettings::on_pushButton_clicked()
{
    QFileDialog dialog;
    dialog.setNameFilter("All Audio files (*.mp3 *.wav *.ogg)");
    dialog.setFilter(QDir::Files);
    dialog.setDirectory(ui->soundFileLineEdit->text());
    dialog.exec();
    ui->soundFileLineEdit->setText(dialog.selectedFiles().first());
}
