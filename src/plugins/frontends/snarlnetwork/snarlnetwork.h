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

#ifndef SNARLNETWORK_H
#define SNARLNETWORK_H
#include "core/plugins/snorefrontend.h"
#include "parser.h"

#include <QPointer>

namespace Snore{
    class Notification;
    class SnoreCore;
}

struct SnarlNotification{
    Snore::Notification notification;
    QString action;
    bool httpClient;
    bool vailid;
    bool isNotification;
    QPointer<class QTcpSocket> clientSocket;
};

class SnarlNetworkFrontend:public Snore::SnoreFrontend{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreFrontend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationFrontend/1.0")
    friend class Parser;
public:
    static const int port=9887;

public:
    SnarlNetworkFrontend();
    ~SnarlNetworkFrontend();
    virtual bool init(Snore::SnoreCore *snore);
    void actionInvoked(Snore::Notification notification);
    void notificationClosed(Snore::Notification notification);

private slots:
    void handleConnection();
    void handleMessages();

private:
    class QTcpServer *tcpServer;
    Parser *parser;
    QHash<uint,SnarlNotification> notifications;
    QHash<QString,Snore::Application*> m_applications;

    void callback(const SnarlNotification &sn,QString msg);

};

#endif //SNARLNETWORK_H
