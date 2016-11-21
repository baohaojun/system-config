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
#include "sound.h"

#include <QtMultimedia/QMediaPlayer>
#include <QTimer>

namespace SnorePlugin {

Sound::Sound():
    m_player(new QMediaPlayer(this))
{
//    connect(m_player,QMediaPlayer::positionChanged,[](qint64 pos){
//        qCDebug(SNORE) << "Player: " << pos;
//    });
    connect(m_player, &QMediaPlayer::stateChanged, [](QMediaPlayer::State state) {
        qCDebug(SNORE) << "Player: " << state;
    });
}

void Sound::setDefaultSettings()
{
    setDefaultSettingsValue(QStringLiteral("Volume"), 50);
    SnoreSecondaryBackend::setDefaultSettings();
}

void Sound::slotNotificationDisplayed(Snore::Notification notification)
{
    if (notification.hints().value("silent").toBool()) {
        return;
    }
    m_player->setVolume(settingsValue(QStringLiteral("Volume")).toInt());

    QString sound = notification.hints().value("sound").toString();
    if (sound.isEmpty()) {
        sound = settingsValue(QStringLiteral("Sound")).toString();
    }
    qCDebug(SNORE) << "SoundFile:" << sound;
    if (!sound.isEmpty()) {
        m_player->setMedia(QUrl::fromLocalFile(sound));
        qCDebug(SNORE) << "SoundFile:" << m_player->media().canonicalUrl();
        m_player->play();
        QTimer *timeout = new QTimer(this);
        timeout->setSingleShot(true);
        timeout->setInterval(notification.timeout() * 1000);
        connect(timeout, &QTimer::timeout, [this, timeout] {
            m_player->stop();
            timeout->deleteLater();
        });
    }
}

}
