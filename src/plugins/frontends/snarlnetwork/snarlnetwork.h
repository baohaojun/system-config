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
#include "libsnore/plugins/snorefrontend.h"
#include "libsnore/log.h"
#include "parser.h"

#include <QTcpSocket>
#include <QTcpServer>

class SnarlNetworkFrontend : public Snore::SnoreFrontend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreFrontend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationFrontend/1.0" FILE "plugin.json")
    friend class Parser;
public:
    static const int port = 9887;

public:
    SnarlNetworkFrontend() = default;
    ~SnarlNetworkFrontend() = default;
    virtual bool initialize() override;
    virtual bool deinitialize() override;

public Q_SLOTS:
    void slotActionInvoked(Snore::Notification notification) override;
    void slotNotificationClosed(Snore::Notification notification) override;

private Q_SLOTS:
    void handleConnection();
    void handleMessages();

private:
    QTcpServer *tcpServer;
    Parser *parser;
    QHash<QTcpSocket *, Snore::Application> m_applications;

    void callback(Snore::Notification &sn, QString msg);

    inline void write(QTcpSocket *dest, const QString &msg)
    {
        snoreDebug(SNORE_DEBUG) << msg;
        dest->write(msg.toLatin1());
    }

};

#endif //SNARLNETWORK_H
