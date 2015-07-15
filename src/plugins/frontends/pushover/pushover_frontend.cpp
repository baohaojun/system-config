/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2014  Patrick von Reth <vonreth@kde.org>

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
#include "pushoversettings.h"

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

void PushoverFrontend::load()
{
    emit loadedStateChanged(true);
}

PluginSettingsWidget *PushoverFrontend::settingsWidget()
{
    return new PushoverSettings(this);
}

void PushoverFrontend::login(const QString &email, const QString &password, const QString &deviceName)
{
    setSettingsValue(QLatin1String("DeviceName"), deviceName, Snore::LOCAL_SETTING);

    QNetworkRequest request(QUrl(QLatin1String("https://api.pushover.net/1/users/login.json")));

    request.setHeader(QNetworkRequest::ContentTypeHeader, QVariant(QLatin1String("application/x-www-form-urlencoded")));
    QNetworkReply *reply = m_manager.post(request, (QLatin1String("email=") + email + QLatin1String("&password=") + password).toUtf8().constData());

    connect(reply, &QNetworkReply::finished, [reply, deviceName, this]() {
        snoreDebug(SNORE_DEBUG) << reply->error();
        QByteArray input = reply->readAll();
        reply->close();
        reply->deleteLater();

        QJsonObject message = QJsonDocument::fromJson(input).object();

        if (message.value(QLatin1String("status")).toInt() == 1) {
            registerDevice(message.value(QLatin1String("secret")).toString(), deviceName);
        } else {
            snoreDebug(SNORE_WARNING) << "An error occure" << input;
            emit loggedInChanged(false);
        }
    });
}

void PushoverFrontend::logOut()
{
    setSettingsValue(QLatin1String("Secret"), QString(), LOCAL_SETTING);
    setSettingsValue(QLatin1String("DeviceID"), QString(), LOCAL_SETTING);
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
    setDefaultSettingsValue(QLatin1String("Secret"), QString(), LOCAL_SETTING);
    setDefaultSettingsValue(QLatin1String("DeviceID"), QString(), LOCAL_SETTING);
    SnoreFrontend::setDefaultSettings();
}

void PushoverFrontend::slotActionInvoked(Notification notification)
{
    if (notification.priority() == Notification::EMERGENCY) {
        snoreDebug(SNORE_WARNING) << "emergeency notification" << notification;
        acknowledgeNotification(notification);
    }
}

QString PushoverFrontend::secret()
{
    return settingsValue(QLatin1String("Secret"),  LOCAL_SETTING).toString();
}

QString PushoverFrontend::device()
{
    return settingsValue(QLatin1String("DeviceID"),  LOCAL_SETTING).toString();
}

void PushoverFrontend::connectToService()
{
    if (secret().isEmpty() || device().isEmpty()) {
        snoreDebug(SNORE_WARNING) << "not logged in";
        return;
    }
    snoreDebug(SNORE_DEBUG) << "Connecting ton service";
    m_socket = new QWebSocket(QString(), QWebSocketProtocol::VersionLatest, this);

    connect(m_socket, &QWebSocket::binaryMessageReceived, [&](const QByteArray & msg) {
        char c = msg.at(0);
        switch (c) {
        case '#':
            snoreDebug(SNORE_DEBUG) << "still alive";
            break;
        case '!':
            getMessages();
            break;
        case 'R':
            snoreDebug(SNORE_DEBUG) << "need to reconnect";
            m_socket->close();
            m_socket->deleteLater();
            connectToService();
            break;
        case 'E':
            snoreDebug(SNORE_WARNING) << "Connection Error";
            emit error(QLatin1String("Please Loggin to https://pushover.net and reanble your device."));
            emit loggedInChanged(false);
            m_socket->close();
            m_socket->deleteLater();
            break;
        default:
            snoreDebug(SNORE_WARNING) << "unknown message recieved" << msg;
        }
    });
    connect(m_socket, &QWebSocket::disconnected, [this]() {
        snoreDebug(SNORE_WARNING) << "disconnected";
        QTimer::singleShot(500, this, &PushoverFrontend::connectToService);
    });
    connect(m_socket, static_cast<void (QWebSocket::*)(QAbstractSocket::SocketError)>(&QWebSocket::error), [&](QAbstractSocket::SocketError error) {
        snoreDebug(SNORE_WARNING) << error << m_socket->errorString();
        emit loggedInChanged(false);
    });
    connect(m_socket, &QWebSocket::connected, [&]() {
        snoreDebug(SNORE_DEBUG) << "connecting";
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
    QNetworkRequest request(QUrl(QLatin1String("https://api.pushover.net/1/devices.json")));

    request.setHeader(QNetworkRequest::ContentTypeHeader, QVariant(QLatin1String("application/x-www-form-urlencoded")));
    QNetworkReply *reply = m_manager.post(request, (QLatin1String("os=O&secret=") + secret + QLatin1String("&name=") + deviceName).toUtf8().constData());

    connect(reply, &QNetworkReply::finished, [reply, secret, this]() {
        snoreDebug(SNORE_DEBUG) << reply->error();
        QByteArray input = reply->readAll();
        reply->close();
        reply->deleteLater();
        QJsonObject message = QJsonDocument::fromJson(input).object();
        if (message.value(QLatin1String("status")).toInt() == 1) {
            setSettingsValue(QLatin1String("Secret"), secret, LOCAL_SETTING);
            setSettingsValue(QLatin1String("DeviceID"), message.value(QLatin1String("id")).toString(), LOCAL_SETTING);;
            connectToService();
        } else {
            snoreDebug(SNORE_WARNING) << "An error occure" << input;
            emit loggedInChanged(false);
            emit error(message.value(QLatin1String("error")).toString());
        }

    });

}

void PushoverFrontend::getMessages()
{
    QNetworkRequest request(QUrl::fromEncoded((QLatin1String("https://api.pushover.net/1/messages.json?"
                            "secret=") + secret() + QLatin1String("&device_id=") + device()).toUtf8().constData()));
    QNetworkReply *reply =  m_manager.get(request);

    connect(reply, &QNetworkReply::finished, [reply, this]() {
        snoreDebug(SNORE_DEBUG) << reply->error();
        QByteArray input = reply->readAll();
        reply->close();
        reply->deleteLater();

        snoreDebug(SNORE_DEBUG) << input;

        QJsonObject message = QJsonDocument::fromJson(input).object();

        int latestID = -1;
        if (message.value(QLatin1String("status")).toInt() == 1) {
            QJsonArray notifications = message.value(QLatin1String("messages")).toArray();
            for (const QJsonValue &v : notifications) {
                QJsonObject notification = v.toObject();

                latestID = qMax(latestID, notification.value(QLatin1String("id")).toInt());

                QString appName = notification.value(QLatin1String("app")).toString();
                Application app = SnoreCore::instance().aplications().value(appName);

                if (!app.isValid()) {
                    Icon icon(QLatin1String("https://api.pushover.net/icons/") +
                              notification.value(QLatin1String("icon")).toString() +
                              QLatin1String(".png"));
                    app = Application(appName, icon);
                    SnoreCore::instance().registerApplication(app);
                }

                Notification n(app, app.defaultAlert(), notification.value(QLatin1String("title")).toString(),
                               notification.value(QLatin1String("message")).toString(),
                               app.icon(), Notification::defaultTimeout(),
                               static_cast<Notification::Prioritys>(notification.value(QLatin1String("priority")).toInt()));
                if (n.priority() == Notification::EMERGENCY) {
                    n.hints().setValue("receipt", notification.value(QLatin1String("receipt")).toString());
                    n.hints().setValue("acked", notification.value(QLatin1String("acked")).toInt());
                }
                if (notification.value(QLatin1String("html")).toInt() == 1) {
                    n.hints().setValue("use-markup", true) ;
                }
                n.data()->setSource(this);
                SnoreCore::instance().broadcastNotification(n);
            }
            if (latestID != -1) {
                deleteMessages(latestID);
            }
        } else {
            snoreDebug(SNORE_WARNING) << "An error occure" << input;
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
        snoreDebug(SNORE_DEBUG) << reply->error();
        snoreDebug(SNORE_DEBUG) << reply->readAll();
        reply->close();
        reply->deleteLater();
    });
}

void PushoverFrontend::acknowledgeNotification(Notification notification)
{
    if (notification.constHints().value("acked").toInt() == 1) {
        return;
    }
    snoreDebug(SNORE_DEBUG) << notification.constHints().value("acked").toInt();
    QString receipt = notification.constHints().value("receipt").toString();

    QNetworkRequest request(QUrl::fromEncoded((QLatin1String("https://api.pushover.net/1/receipts/") +
                            receipt + QLatin1String("/acknowledge.json")).toUtf8().constData()));
    snoreDebug(SNORE_WARNING) << request.url();
    request.setHeader(QNetworkRequest::ContentTypeHeader, QVariant(QLatin1String("application/x-www-form-urlencoded")));
    QNetworkReply *reply = m_manager.post(request, (QLatin1String("secret=") + secret()).toUtf8().constData());

    connect(reply, &QNetworkReply::finished, [reply]() {
        snoreDebug(SNORE_DEBUG) << reply->error();
        snoreDebug(SNORE_DEBUG) << reply->readAll();
        //TODO:parse reply
        reply->close();
        reply->deleteLater();
    });
}

