/****************************************************************************************
 * Copyright (c) 2010 Patrick von Reth <patrick.vonreth@gmail.com>                      *
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

#ifndef REDIRECTOR_H
#define REDIRECTOR_H
#include "core/interface.h"
#include <QHash>
#include "webinterface/webinterface.h"

class Redircetor:public Notification_Backend,WebInterface_Plugin{
    Q_OBJECT
    Q_INTERFACES(Notification_Backend WebInterface_Plugin)
public:
    static const int port=9887;
public:
    Redircetor();
    bool isPrimaryNotificationBackend(){return false;}
    QString display();
    bool parseCommand(QTcpSocket *client, const QString &command);
    class SnoreServer* getSnore();
    void setSnore(class SnoreServer *snore);

public slots:
        int notify(QSharedPointer<class Notification>notification);
        void closeNotification(int nr);

private:
    QHash<QString,QSharedPointer<QTcpSocket> > subscribers;
    QPointer<class SnoreServer> snore;

    enum ARGUMENTS{
        SUBSCRIBE=1,
        UNSUBSCRIBE=2,
        LISTSUBSCRIBERS=3,
    };
    QHash<QString,Redircetor::ARGUMENTS> getArgument;

};


#endif//REDIRECTOR_H
