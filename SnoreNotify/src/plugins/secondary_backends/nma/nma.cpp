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
#include "nma.h"

#include "libsnore/utils.h"

#include <QNetworkReply>
#include <QNetworkRequest>

namespace SnorePlugin {

void NMA::slotNotify(Snore::Notification notification)
{
    QString key = settingsValue(QStringLiteral("ApiKey")).toString();
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
        data += notification.text(Snore::Utils::Href | Snore::Utils::Bold | Snore::Utils::Break |
                                  Snore::Utils::Underline | Snore::Utils::Font | Snore::Utils::Italic)
                + QLatin1String("&content-type=text/html");
    } else {
        data += notification.text();
    }

    QNetworkReply *reply =  m_manager.post(request, data.toUtf8().constData());
    connect(reply, &QNetworkReply::finished, [reply]() {
        qCDebug(SNORE) << reply->error();
        qCDebug(SNORE) << reply->readAll();
        reply->close();
        reply->deleteLater();
    });

}

void NMA::setDefaultSettings()
{
    setDefaultSettingsValue(QStringLiteral("ApiKey"), QString());
    SnoreSecondaryBackend::setDefaultSettings();
}

}
