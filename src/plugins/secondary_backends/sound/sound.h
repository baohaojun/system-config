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
#ifndef SOUND_H
#define SOUND_H

#include "libsnore/plugins/snorebackend.h"

class QMediaPlayer;

class Sound : public Snore::SnoreSecondaryBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreSecondaryBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.SecondaryNotificationBackend/1.0" FILE "plugin.json")
public:
    Sound();
    ~Sound() = default;

    virtual bool initialize() override;

    Snore::PluginSettingsWidget *settingsWidget() override;

public Q_SLOTS:
    void slotNotificationDisplayed(Snore::Notification notification) override;
private:
    QMediaPlayer *m_player;
};

#endif // SOUND_H
