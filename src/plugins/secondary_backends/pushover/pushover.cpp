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

#include "libsnore/utils.h"
#include "libsnore/notification/notification_p.h"

#include <QNetworkReply>
#include <QNetworkRequest>
#include <QHttpMultiPart>

using namespace Snore;

void Pushover::slotNotify(Notification notification)
{
    if (notification.data()->sourceAndTargetAreSimilar(this)) {
        return;
    }

    QString key = settingsValue(QLatin1String("UserKey")).toString();
    if (key.isEmpty()) {
        return;
    }

    QNetworkRequest request(QUrl::fromEncoded("https://api.pushover.net/1/messages.json"));
    QHttpMultiPart *mp = new QHttpMultiPart(QHttpMultiPart::FormDataType);

    QHttpPart title;
    title.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"title\"")));
    title.setBody(notification.title().toUtf8().constData());
    mp->append(title);

    QString textString;
    if (notification.constHints().value("use-markup").toBool()) {
        textString = notification.text(Utils::HREF | Utils::BOLD | Utils::UNDERLINE | Utils::FONT | Utils::ITALIC);

        QHttpPart html;
        html.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"html\"")));
        html.setBody("1");
        mp->append(html);
    } else {
        textString = notification.text();
    }

    QHttpPart text;
    text.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"message\"")));
    snoreDebug(SNORE_DEBUG) << "Use Markup" << notification.constHints().value("use-markup").toBool();
    snoreDebug(SNORE_DEBUG) << "Message" << textString;
    text.setBody(textString.toUtf8().constData());
    mp->append(text);

    QHttpPart priority;
    priority.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"priority\"")));
    priority.setBody(QString::number(notification.priority()).toUtf8().constData());
    mp->append(priority);
    if (notification.priority() == Notification::EMERGENCY) {

        QHttpPart retry;
        retry.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"retry\"")));
        retry.setBody("30");// rety every 30 s
        mp->append(retry);

        QHttpPart expire;
        expire.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"expire\"")));
        expire.setBody("300");//5 min
        mp->append(expire);
    }

    QHttpPart sound;
    sound.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"sound\"")));
    if (notification.hints().value("silent").toBool()) {
        sound.setBody("none");
    } else {
        sound.setBody(settingsValue(QLatin1String("Sound"), LOCAL_SETTING).toString().toUtf8().constData());
    }
    mp->append(sound);

    if (!settingsValue(QLatin1String("Devices"), LOCAL_SETTING).toString().isEmpty()) {
        QHttpPart devices;
        devices.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"device\"")));
        devices.setBody(settingsValue(QLatin1String("Devices"), LOCAL_SETTING).toString().toUtf8().constData());
        mp->append(devices);
    }

    QHttpPart token;
    token.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"token\"")));
    token.setBody(notification.application().constHints().value("pushover-token").toString().toUtf8().constData());
    mp->append(token);

    QHttpPart user;
    user.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"user\"")));
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

void Pushover::slotInitialize()
{
    emit initializeChanged(true);
}

PluginSettingsWidget *Pushover::settingsWidget()
{
    return new PushoverSettings(this);
}

void Pushover::setDefaultSettings()
{
    setDefaultSettingsValue(QLatin1String("UserKey"), QString());
    setDefaultSettingsValue(QLatin1String("Sound"), QLatin1String("pushover"), LOCAL_SETTING);
    setDefaultSettingsValue(QLatin1String("Devices"), QString(), LOCAL_SETTING);
    SnoreSecondaryBackend::setDefaultSettings();
}
