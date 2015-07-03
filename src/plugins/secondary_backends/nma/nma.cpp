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
#include "nma.h"
#include "nmasettings.h"

#include"libsnore/utils.h"

#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>

using namespace Snore;

void NotifyMyAndroid::slotNotify(Notification notification)
{
    QString key = settingsValue(QLatin1String("ApiKey")).toString();
    if (key.isEmpty()) {
        return;
    }

    QNetworkRequest request(QUrl::fromEncoded("https://www.notifymyandroid.com/publicapi/notify"));
    request.setHeader(QNetworkRequest::ContentTypeHeader, QVariant(QLatin1String("application/x-www-form-urlencoded")));

    QString data = QLatin1String("apikey=") + key
                   + QLatin1String("&application=") + notification.application().name()
                   + QLatin1String("&event=") + notification.title()
                   + QLatin1String("&priority=") + QString::number(notification.priority())
                   + QLatin1String("&description=");

    if (notification.constHints().value("supports-markup").toBool()) {
        data += notification.text(Utils::HREF | Utils::BOLD | Utils::BREAK |
                                  Utils::UNDERLINE | Utils::FONT | Utils::ITALIC)
                + QLatin1String("&content-type=text/html");
    } else {
        data += notification.text();
    }

    QNetworkReply *reply =  m_manager.post(request, data.toUtf8().constData());
    connect(reply, &QNetworkReply::finished, [reply]() {
        snoreDebug(SNORE_DEBUG) << reply->error();
        snoreDebug(SNORE_DEBUG) << reply->readAll();
        reply->close();
        reply->deleteLater();
    });

}

bool NotifyMyAndroid::initialize()
{
    setDefaultSettingsValue(QLatin1String("ApiKey"), QString());
    return SnoreSecondaryBackend::initialize();
}

PluginSettingsWidget *NotifyMyAndroid::settingsWidget()
{
    return new NotifyMyAndroidSettings(this);
}
