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
#ifndef PUSHOVERCLIENT_H
#define PUSHOVERCLIENT_H

#include "libsnore/notification/notification.h"

#include <QNetworkAccessManager>
#include <QPointer>

class QWebSocket;

class PushoverFrontend;

class PushoverClient : public QObject
{
    Q_OBJECT
public:
    enum LoginState {
        LoggedIn,
        LoggedOut,
        Error,
        Undefined
    };

    PushoverClient(PushoverFrontend *frontend);

    void logOut();
    void login(const QString &email, const QString &password, const QString &deviceName);
    void disconnectService();

    void acknowledgeNotification(Snore::Notification notification);

    LoginState isLoggedIn() const;
    QString errorMessage();

public Q_SLOTS:
    void connectToService();

Q_SIGNALS:
    void loggedInChanged(LoginState isLoggedIn);
    void error(const QString &error);

private:
    PushoverFrontend *m_frontend;
    QNetworkAccessManager m_manager;
    QPointer<QWebSocket> m_socket;
    LoginState m_loggedIn = LoggedOut;
    QString m_errorMessage;

    QString secret();
    QString device();

    void registerDevice(const QString &secret, const QString &deviceName);
    void getMessages();
    void deleteMessages(int latestMessageId);
};

#endif // PUSHOVERCLIENT_H
