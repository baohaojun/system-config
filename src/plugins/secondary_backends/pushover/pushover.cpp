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
#include "pushover.h"
#include "pushoversettings.h"

#include"libsnore/utils.h"

#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QHttpMultiPart>

using namespace Snore;

void Pushover::slotNotify(Notification notification)
{
    QString key = value("UserKey").toString();
    if (key.isEmpty()) {
        return;
    }

    QNetworkRequest request(QUrl("https://api.pushover.net/1/messages.json"));
    QHttpMultiPart *mp = new QHttpMultiPart(QHttpMultiPart::FormDataType);

    QHttpPart title;
    title.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant("form-data; name=\"title\""));
    title.setBody(notification.title().toUtf8().constData());
    mp->append(title);

    QString textString;
    if(notification.application().constHints().value("use-markup").toBool()){
        textString = notification.text(Utils::HREF | Utils::BOLD | Utils::UNDERLINE | Utils::FONT | Utils::ITALIC);

        QHttpPart html;
        html.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant("form-data; name=\"html\""));
        html.setBody("1");
        mp->append(html);
    } else {
        textString = notification.text();
    }

    QHttpPart text;
    text.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant("form-data; name=\"message\""));
    snoreDebug(SNORE_DEBUG) << "Message" << textString;
    text.setBody(textString.toUtf8().constData());
    mp->append(text);

    QHttpPart priority;
    priority.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant("form-data; name=\"priority\""));
    priority.setBody(QString::number(notification.priority()).toUtf8().constData());
    mp->append(priority);

    QHttpPart sound;
    sound.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant("form-data; name=\"sound\""));
    if (notification.hints().value("silent").toBool()) {
        sound.setBody("none");
    } else {
        sound.setBody(value("Sound", LOCAL_SETTING).toString().toUtf8().constData());
    }
    mp->append(sound);

    if (!value("Devices", LOCAL_SETTING).toString().isEmpty()) {
        QHttpPart devices;
        devices.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant("form-data; name=\"device\""));
        devices.setBody(value("Devices", LOCAL_SETTING).toString().toUtf8().constData());
        mp->append(devices);
    }

    QHttpPart token;
    token.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant("form-data; name=\"token\""));
    token.setBody(notification.application().constHints().value("pushover-token").toString().toUtf8().constData());
    mp->append(token);

    QHttpPart user;
    user.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant("form-data; name=\"user\""));
    user.setBody(key.toUtf8().constData());
    mp->append(user);

    QNetworkReply *reply =  m_manager.post(request, mp);
    mp->setParent(reply);

    connect(reply, &QNetworkReply::finished, [reply]() {
        snoreDebug(SNORE_DEBUG) << reply->error();
        snoreDebug(SNORE_DEBUG) << reply->readAll();
        reply->close();
        reply->deleteLater();
    });

}

bool Pushover::initialize()
{
    setDefaultValue("UserKey", "");
    setDefaultValue("Sound", "pushover", LOCAL_SETTING);
    setDefaultValue("Devices", "", LOCAL_SETTING);
    return SnoreSecondaryBackend::initialize();
}

PluginSettingsWidget *Pushover::settingsWidget()
{
    return new PushoverSettings(this);
}
