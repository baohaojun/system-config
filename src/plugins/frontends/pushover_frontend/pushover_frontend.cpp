/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2015  Hannah von Reth <vonreth@kde.org>

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

#include "pushover_frontend.h"

#include "libsnore/snore.h"
#include "libsnore/version.h"
#include "libsnore/notification/notification_p.h"

#include <QNetworkReply>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>
#include <QtWebSockets>

using namespace Snore;

// TODO: use qtkeychain to encrypt credentials?
// TODO: massive refactoring ...

PushoverFrontend::PushoverFrontend()
{
    connect(this, &PushoverFrontend::loggedInChanged, [this](bool state) {
        m_loggedIn = state;
    });
    connect(this, &PushoverFrontend::error, [this](QString error) {
        m_errorMessage = error;
    });
    connect(this, &PushoverFrontend::enabledChanged, [this](bool enabled) {
        if (enabled) {
            connectToService();
        } else {
            disconnectService();
        }
    });
}

void PushoverFrontend::login(const QString &email, const QString &password, const QString &deviceName)
{
    setSettingsValue(QStringLiteral("DeviceName"), deviceName, Snore::LocalSetting);

    QNetworkRequest request(QUrl(QStringLiteral("https://api.pushover.net/1/users/login.json")));

    request.setHeader(QNetworkRequest::ContentTypeHeader, QVariant(QLatin1String("application/x-www-form-urlencoded")));
    QNetworkReply *reply = m_manager.post(request, (QLatin1String("email=") + email + QLatin1String("&password=") + password).toUtf8().constData());

    connect(reply, &QNetworkReply::finished, [reply, deviceName, this]() {
        qCDebug(SNORE) << reply->error();
        QByteArray input = reply->readAll();
        reply->close();
        reply->deleteLater();

        QJsonObject message = QJsonDocument::fromJson(input).object();

        if (message.value(QStringLiteral("status")).toInt() == 1) {
            registerDevice(message.value(QStringLiteral("secret")).toString(), deviceName);
        } else {
            qCWarning(SNORE) << "An error occure" << input;
            emit loggedInChanged(false);
        }
    });
}

void PushoverFrontend::logOut()
{
    setSettingsValue(QStringLiteral("Secret"), QString(), LocalSetting);
    setSettingsValue(QStringLiteral("DeviceID"), QString(), LocalSetting);
    m_socket->close();
    m_socket->deleteLater();
    emit loggedInChanged(false);
}

bool PushoverFrontend::isLoggedIn() const
{
    return m_loggedIn;
}

QString PushoverFrontend::errorMessage()
{
    return m_errorMessage;
}

void PushoverFrontend::setDefaultSettings()
{
    setDefaultSettingsValue(QStringLiteral("Secret"), QString(), LocalSetting);
    setDefaultSettingsValue(QStringLiteral("DeviceID"), QString(), LocalSetting);
    SnoreFrontend::setDefaultSettings();
}

void PushoverFrontend::slotActionInvoked(Notification notification)
{
    if (notification.priority() == Notification::Emergency) {
        qCWarning(SNORE) << "emergeency notification" << notification;
        acknowledgeNotification(notification);
    }
}

QString PushoverFrontend::secret()
{
    return settingsValue(QStringLiteral("Secret"),  LocalSetting).toString();
}

QString PushoverFrontend::device()
{
    return settingsValue(QStringLiteral("DeviceID"),  LocalSetting).toString();
}

void PushoverFrontend::connectToService()
{
    if (secret().isEmpty() || device().isEmpty()) {
        qCWarning(SNORE) << "not logged in";
        return;
    }
    qCDebug(SNORE) << "Connecting ton service";
    m_socket = new QWebSocket(QString(), QWebSocketProtocol::VersionLatest, this);

    connect(m_socket.data(), &QWebSocket::binaryMessageReceived, [&](const QByteArray & msg) {
        char c = msg.at(0);
        switch (c) {
        case '#':
            qCDebug(SNORE) << "still alive";
            break;
        case '!':
            getMessages();
            break;
        case 'R':
            qCDebug(SNORE) << "need to reconnect";
            m_socket->close();
            m_socket->deleteLater();
            connectToService();
            break;
        case 'E':
            qCWarning(SNORE) << "Connection Error";
            emit error(QStringLiteral("Please Loggin to https://pushover.net and reanble your device."));
            emit loggedInChanged(false);
            m_socket->close();
            m_socket->deleteLater();
            break;
        default:
            qCWarning(SNORE) << "unknown message received" << msg;
        }
    });
    connect(m_socket.data(), &QWebSocket::disconnected, [this]() {
        qCWarning(SNORE) << "disconnected";
        //TODO: use new style connect once we depend on qt 5.4
        QTimer::singleShot(500, this, SLOT(PushoverFrontend::connectToService()));
    });
    connect(m_socket.data(), static_cast<void (QWebSocket::*)(QAbstractSocket::SocketError)>(&QWebSocket::error), [&](QAbstractSocket::SocketError error) {
        qCWarning(SNORE) << error << m_socket->errorString();
        emit loggedInChanged(false);
    });
    connect(m_socket.data(), &QWebSocket::connected, [&]() {
        qCDebug(SNORE) << "connecting";
        m_socket->sendBinaryMessage((QLatin1String("login:") + device() + QLatin1Char(':') + secret() + QLatin1Char('\n')).toUtf8().constData());
        emit loggedInChanged(true);
        getMessages();
    });
    m_socket->open(QUrl::fromEncoded("wss://client.pushover.net/push"));
}

void PushoverFrontend::disconnectService()
{
    if (m_socket) {
        m_socket->close();
        m_socket->deleteLater();
    }
}

void PushoverFrontend::registerDevice(const QString &secret, const QString &deviceName)
{
    QNetworkRequest request(QUrl(QStringLiteral("https://api.pushover.net/1/devices.json")));

    request.setHeader(QNetworkRequest::ContentTypeHeader, QVariant(QLatin1String("application/x-www-form-urlencoded")));
    QNetworkReply *reply = m_manager.post(request, (QLatin1String("os=O&secret=") + secret + QLatin1String("&name=") + deviceName).toUtf8().constData());

    connect(reply, &QNetworkReply::finished, [reply, secret, this]() {
        qCDebug(SNORE) << reply->error();
        QByteArray input = reply->readAll();
        reply->close();
        reply->deleteLater();
        QJsonObject message = QJsonDocument::fromJson(input).object();
        if (message.value(QStringLiteral("status")).toInt() == 1) {
            setSettingsValue(QStringLiteral("Secret"), secret, LocalSetting);
            setSettingsValue(QStringLiteral("DeviceID"), message.value(QStringLiteral("id")).toString(), LocalSetting);;
            connectToService();
        } else {
            qCWarning(SNORE) << "An error occure" << input;
            emit loggedInChanged(false);
            emit error(message.value(QStringLiteral("error")).toString());
        }

    });

}

void PushoverFrontend::getMessages()
{
    QNetworkRequest request(QUrl::fromEncoded((QLatin1String("https://api.pushover.net/1/messages.json?"
                            "secret=") + secret() + QLatin1String("&device_id=") + device()).toUtf8().constData()));
    QNetworkReply *reply =  m_manager.get(request);

    connect(reply, &QNetworkReply::finished, [reply, this]() {
        qCDebug(SNORE) << reply->error();
        QByteArray input = reply->readAll();
        reply->close();
        reply->deleteLater();

        qCDebug(SNORE) << input;

        QJsonObject message = QJsonDocument::fromJson(input).object();

        int latestID = -1;
        if (message.value(QStringLiteral("status")).toInt() == 1) {
            QJsonArray notifications = message.value(QStringLiteral("messages")).toArray();
            foreach(const QJsonValue & v, notifications) {
                QJsonObject notification = v.toObject();

                latestID = qMax(latestID, notification.value(QStringLiteral("id")).toInt());

                QString appName = notification.value(QStringLiteral("app")).toString();
                Application app = SnoreCore::instance().aplications().value(appName);

                if (!app.isValid()) {
                    Icon icon(Icon::fromWebUrl(QUrl::fromEncoded((QLatin1String("https://api.pushover.net/icons/") +
                                               notification.value(QStringLiteral("icon")).toString() +
                                               QLatin1String(".png")).toUtf8().constData())));
                    app = Application(appName, icon);
                    SnoreCore::instance().registerApplication(app);
                }

                Notification n(app, app.defaultAlert(), notification.value(QStringLiteral("title")).toString(),
                               notification.value(QStringLiteral("message")).toString(),
                               app.icon(), Notification::defaultTimeout(),
                               static_cast<Notification::Prioritys>(notification.value(QStringLiteral("priority")).toInt()));
                if (n.priority() == Notification::Emergency) {
                    n.hints().setValue("receipt", notification.value(QStringLiteral("receipt")).toString());
                    n.hints().setValue("acked", notification.value(QStringLiteral("acked")).toInt());
                }
                if (notification.value(QStringLiteral("html")).toInt() == 1) {
                    n.hints().setValue("use-markup", true) ;
                }
                n.data()->setSource(this);
                SnoreCore::instance().broadcastNotification(n);
            }
            if (latestID != -1) {
                deleteMessages(latestID);
            }
        } else {
            qCWarning(SNORE) << "An error occure" << input;
        }

    });

}

void PushoverFrontend::deleteMessages(int latestMessageId)
{

    QNetworkRequest request(QUrl::fromEncoded((QLatin1String("https://api.pushover.net/1/devices/") +
                            device() + QLatin1String("/update_highest_message.json")).toUtf8().constData()));

    request.setHeader(QNetworkRequest::ContentTypeHeader, QVariant(QLatin1String("application/x-www-form-urlencoded")));
    QNetworkReply *reply = m_manager.post(request, (QLatin1String("secret=") + secret() + QLatin1String("&message=") + QString::number(latestMessageId)).toUtf8().constData());

    connect(reply, &QNetworkReply::finished, [reply]() {
        qCDebug(SNORE) << reply->error();
        qCDebug(SNORE) << reply->readAll();
        reply->close();
        reply->deleteLater();
    });
}

void PushoverFrontend::acknowledgeNotification(Notification notification)
{
    if (notification.constHints().value("acked").toInt() == 1) {
        return;
    }
    qCDebug(SNORE) << notification.constHints().value("acked").toInt();
    QString receipt = notification.constHints().value("receipt").toString();

    QNetworkRequest request(QUrl::fromEncoded((QLatin1String("https://api.pushover.net/1/receipts/") +
                            receipt + QLatin1String("/acknowledge.json")).toUtf8().constData()));
    qCWarning(SNORE) << request.url();
    request.setHeader(QNetworkRequest::ContentTypeHeader, QVariant(QLatin1String("application/x-www-form-urlencoded")));
    QNetworkReply *reply = m_manager.post(request, (QLatin1String("secret=") + secret()).toUtf8().constData());

    connect(reply, &QNetworkReply::finished, [reply]() {
        qCDebug(SNORE) << reply->error();
        qCDebug(SNORE) << reply->readAll();
        //TODO:parse reply
        reply->close();
        reply->deleteLater();
    });
}

