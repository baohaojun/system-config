/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2014-2015  Hannah von Reth <vonreth@kde.org>

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
#include "toasty.h"

#include "libsnore/utils.h"

#include <QNetworkReply>
#include <QNetworkRequest>
#include <QHttpMultiPart>
#include <QFile>

using namespace Snore;

void Toasty::slotNotify(Notification notification)
{
    QString key = settingsValue(QStringLiteral("DeviceID")).toString();
    if (key.isEmpty()) {
        return;
    }
    QNetworkRequest request(QUrl::fromUserInput(QLatin1String("http://api.supertoasty.com/notify/") + key));
    QHttpMultiPart *mp = new QHttpMultiPart(QHttpMultiPart::FormDataType);

    QHttpPart title;
    title.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"title\"")));
    title.setBody(notification.title().toUtf8().constData());
    mp->append(title);

    QHttpPart text;
    text.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"text\"")));
    text.setBody(notification.text().toUtf8().constData());
    mp->append(text);

    QHttpPart app;
    app.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"sender\"")));
    app.setBody(notification.application().name().toUtf8().constData());
    mp->append(app);

    QHttpPart icon;

    icon.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QLatin1String("form-data; name=\"image\"; filename=\"") + notification.icon().localUrl(QSize(128, 128)) + QLatin1Char('"')));
    icon.setHeader(QNetworkRequest::ContentTypeHeader, QVariant(QLatin1String("image/png")));
    QFile *file = new QFile(notification.icon().localUrl(QSize(128, 128)));
    file->open(QIODevice::ReadOnly);
    icon.setBodyDevice(file);
    mp->append(icon);

    QNetworkReply *reply =  m_manager.post(request, mp);
    mp->setParent(reply);
    file->setParent(reply);

    connect(reply, &QNetworkReply::finished, [reply]() {
        qCDebug(SNORE) << reply->error();
        qCDebug(SNORE) << reply->readAll();
        reply->close();
        reply->deleteLater();
    });

}

void Toasty::setDefaultSettings()
{
    setDefaultSettingsValue(QStringLiteral("DeviceID"), QString());
    SnoreSecondaryBackend::setDefaultSettings();
}
