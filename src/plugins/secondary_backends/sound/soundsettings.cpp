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
#include "plugins/plugins.h"

#include <QFileDialog>
#include <QLineEdit>
#include <QPushButton>

using namespace Snore;

SoundSettings::SoundSettings(SnorePlugin *snorePlugin, QWidget *parent) :
    PluginSettingsWidget(snorePlugin, parent),
    m_lineEdit(new QLineEdit)
{
    addRow("Sound File:", m_lineEdit);
    QPushButton *button = new QPushButton("Select a sound File");
    connect(button, &QPushButton::clicked, [this]() {
        QFileDialog dialog;
        dialog.setNameFilter("All Audio files (*.mp3 *.wav *.ogg)");
        dialog.setFilter(QDir::Files);
        dialog.setDirectory(m_lineEdit->text());
        dialog.exec();
        m_lineEdit->setText(dialog.selectedFiles().first());
    });
    addRow("", button);
}

SoundSettings::~SoundSettings()
{
}

void SoundSettings::load()
{
    m_lineEdit->setText(m_snorePlugin->value("Sound").toString());
}

void SoundSettings::save()
{
    m_snorePlugin->setValue("Sound", m_lineEdit->text());
}

