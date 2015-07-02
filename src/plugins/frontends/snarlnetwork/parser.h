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

#ifndef PARSER_H
#define PARSER_H
#include <QString>
#include <QHash>
#include "libsnore/notification/notification.h"
#include <QSharedPointer>
#include <QObject>

class Parser : public QObject
{
    Q_OBJECT
public:
    Parser(class SnarlNetworkFrontend *snarl);

    void parse(Snore::Notification &sNotification, const QString &msg, class QTcpSocket *client);

private:
    class SnarlNetworkFrontend *snarl;

    enum snpTypes {
        TYPE,
        APP,
        VERSION,
        ACTION,
        REGISTER,
        ADD_CLASS,
        NOTIFICATION,
        UNREGISTER,
        CLASS,
        TITLE,
        TEXT,
        ICON,
        TIMEOUT,
        ERROR

    };

    QHash<QByteArray, Parser::snpTypes> getSnpType;

};

#endif // PARSER_H
