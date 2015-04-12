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
#include "sound.h"
#include "soundsettings.h"

#include <QtMultimedia/QMediaPlayer>
#include <QTimer>

using namespace Snore;

Sound::Sound():
    SnoreSecondaryBackend("Sound", false),
    m_player(new QMediaPlayer(this))
{
    setDefaultValue("Volume",50);
    m_player->setVolume(value("Volume").toInt());
//    connect(m_player,QMediaPlayer::positionChanged,[](qint64 pos){
//        snoreDebug(SNORE_DEBUG) << "Player: " << pos;
//    });
    connect(m_player, &QMediaPlayer::stateChanged, [](QMediaPlayer::State state) {
        snoreDebug(SNORE_DEBUG) << "Player: " << state;
    });
}

Sound::~Sound()
{

}

PluginSettingsWidget *Sound::settingsWidget()
{
    return new SoundSettings(this);
}

void Sound::slotNotify(Snore::Notification notification)
{
    if (notification.hints().value("silent", false).toBool()) {
        return;
    }

    QString sound = notification.hints().value("sound").toString();
    if (sound.isEmpty()) {
        sound = value("Sound").toString();
    }
    snoreDebug(SNORE_DEBUG) << "SoundFile:" << sound;
    if (!sound.isEmpty()) {
        m_player->setMedia(QUrl::fromLocalFile(sound));
        snoreDebug(SNORE_DEBUG) << "SoundFile:" << m_player->media().canonicalUrl();
        m_player->play();
        QTimer *timeout = new QTimer(this);
        timeout->setSingleShot(true);
        timeout->setInterval(notification.timeout() * 1000);
        connect(timeout, &QTimer::timeout, [this,timeout]{
           m_player->stop();
           timeout->deleteLater();
        });
    }
}
