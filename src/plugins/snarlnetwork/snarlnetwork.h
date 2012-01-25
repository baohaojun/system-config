/****************************************************************************************
 * Copyright (c) 2010-2012 Patrick von Reth <patrick.vonreth@gmail.com>                 *
 *                                                                                      *
 * This program is free software; you can redistribute it and/or modify it under        *
 * the terms of the GNU General Public License as published by the Free Software        *
 * Foundation; either version 2 of the License, or (at your option) any later           *
 * version.                                                                             *
 *                                                                                      *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY      *
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A      *
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.             *
 *                                                                                      *
 * You should have received a copy of the GNU General Public License along with         *
 * this program.  If not, see <http://www.gnu.org/licenses/>.                           *
 ****************************************************************************************/

#ifndef SNARLNETWORK_H
#define SNARLNETWORK_H
#include "core/interface.h"
#include "parser.h"

#include <QPointer>

namespace Snore{
    class Notification;
    class SnoreServer;
}

struct SnarlNotification{
    Snore::Notification notification;
    QString action;
    bool httpClient;
    bool vailid;
    bool isNotification;
    QPointer<class QTcpSocket> clientSocket;
};

class SnarlNetworkFrontend:public Snore::Notification_Frontend{
    Q_OBJECT
    Q_INTERFACES(Snore::Notification_Frontend)
    friend class Parser;
public:
    static const int port=9887;

public:
    SnarlNetworkFrontend();
    ~SnarlNetworkFrontend();
    virtual void init(Snore::SnoreServer *snore);
    void actionInvoked(Snore::Notification notification);
    void notificationClosed(Snore::Notification notification);

private slots:
    void handleConnection();
    void handleMessages();

private:
    class QTcpServer *tcpServer;
    Parser *parser;
    QHash<uint,SnarlNotification> notifications;

    void callback(const SnarlNotification &sn,QString msg);

};

#endif //SNARLNETWORK_H
