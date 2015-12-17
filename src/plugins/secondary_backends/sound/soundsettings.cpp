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
#include "soundsettings.h"
#include "plugins/plugins.h"

#include <QFileDialog>
#include <QLineEdit>
#include <QSpinBox>
#include <QPushButton>

using namespace Snore;

SoundSettings::SoundSettings(SnorePlugin* snorePlugin, QWidget* parent) :
    PluginSettingsWidget(snorePlugin, parent),
    m_lineEditFileName(new QLineEdit),
    m_spinBoxVolume(new QSpinBox)
{
    m_spinBoxVolume->setRange(0, 100);
    addRow(tr("Volume:"), m_spinBoxVolume);

    addRow(tr("Audio file:"), m_lineEditFileName, tr("The sound played when a notification is received."));
    QPushButton* button = new QPushButton(tr("Select a audio file"));
    connect(button, &QPushButton::clicked, [this]() {
        QFileDialog dialog;
        dialog.setNameFilter(tr("All Audio files").append(QLatin1String("(*.mp3 *.wav *.ogg)")));
        dialog.setFileMode(QFileDialog::ExistingFile);
        dialog.setDirectory(m_lineEditFileName->text());
        if (dialog.exec()) {
            QStringList files = dialog.selectedFiles();
            m_lineEditFileName->setText(files.first());
        }
    });
    addRow(QString(), button);
}

SoundSettings::~SoundSettings()
{
}

void SoundSettings::load()
{
    m_lineEditFileName->setText(settingsValue(QStringLiteral("Sound")).toString());
    m_spinBoxVolume->setValue(settingsValue(QStringLiteral("Volume")).toInt());
}

void SoundSettings::save()
{
    setSettingsValue(QStringLiteral("Sound"), m_lineEditFileName->text());
    setSettingsValue(QStringLiteral("Volume"), m_spinBoxVolume->value());
}

