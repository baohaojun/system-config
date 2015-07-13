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

#ifndef PUSHOVER_FRONTEND_H
#define PUSHOVER_FRONTEND_H
#include "libsnore/plugins/snorefrontend.h"
#include "libsnore/application.h"

#include <QNetworkAccessManager>
#include <QPointer>

class QWebSocket;

class PushoverFrontend : public Snore::SnoreFrontend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreFrontend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationFrontend/1.0" FILE "plugin.json")
public:
    PushoverFrontend();
    ~PushoverFrontend() = default;

    Snore::PluginSettingsWidget *settingsWidget() override;

    void login(const QString &email, const QString &password, const QString &deviceName);
    void logOut();

    bool isLoggedIn() const;
    QString errorMessage();

protected:
    void setDefaultSettings() override;

public Q_SLOTS:
    void slotInitialize() override;
    void slotActionInvoked(Snore::Notification notification);

Q_SIGNALS:
    void loggedInChanged(bool isLoggedIn);
    void error(QString error);

private:
    QNetworkAccessManager m_manager;
    QPointer<QWebSocket> m_socket;
    bool m_loggedIn = false;
    QString m_errorMessage;

    QString secret();
    QString device();

    void connectToService();
    void disconnectService();

    void registerDevice(const QString &secret, const QString &deviceName);
    void getMessages();
    void deleteMessages(int latestMessageId);
    void acknowledgeNotification(Snore::Notification notification);

};

#endif//PUSHOVER_FRONTEND_H
