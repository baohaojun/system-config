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

#include "parser.h"
#include "snarlnetwork.h"

#include "core/snore.h"
#include "core/notification/notification.h"
#include "core/notification/notification_p.h"

#include <QDir>
#include <QNetworkAccessManager>
#include <QEventLoop>
#include <QNetworkReply>
#include <QObject>
#include <QTcpSocket>

using namespace Snore;

Parser::Parser(SnarlNetworkFrontend *snarl):
    QObject(snarl),
    snarl(snarl)
{
    getSnpType.insert("type", TYPE);
    getSnpType.insert("app", APP);
    getSnpType.insert("version", VERSION);
    getSnpType.insert("action", ACTION);
    getSnpType.insert("register", REGISTER);
    getSnpType.insert("add_class", ADD_CLASS);
    getSnpType.insert("notification", NOTIFICATION);
    getSnpType.insert("unregister", UNREGISTER);
    getSnpType.insert("class", CLASS);
    getSnpType.insert("title", TITLE);
    getSnpType.insert("text", TEXT);
    getSnpType.insert("icon", ICON);
    getSnpType.insert("timeout", TIMEOUT);
}

void Parser::parse(Notification &sNotification, const QString &msg, QTcpSocket *client)
{
    snoreDebug(SNORE_DEBUG) << msg;
    QStringList splitted(msg.split("#?"));
    snpTypes action(ERROR);

    QString appName;
    QString alertName;
    QString title;
    QString text;
    Icon icon(":/root/snore.png");
    int timeout = Notification::defaultTimeout();

    QString key;
    QString value;

    foreach(QString s, splitted) {
        key = s.mid(0, s.indexOf("=")).toLower();
        value = s.mid(s.indexOf("=") + 1);
        switch (getSnpType.value(key)) {
        case APP:
            appName = value;
            break;
        case ACTION:
            action = getSnpType.value(value);
            break;
        case  TITLE:
            title = value;
            break;
        case TEXT:
            text = value;
            break;
        case ICON:
            icon = Icon(value);
            break;
        case CLASS:
            alertName = value;
            break;
        case TIMEOUT:
            timeout = value.toInt();
            break;
        default:
            break;
        }
    }

    Application app;
    Alert alert;

    if (snarl->m_applications.contains(client)) {
        app = snarl->m_applications[client];
    }

    if (!alertName.isEmpty() && app.isValid()) {
        if (app.alerts().contains(alertName)) {
            alert = app.alerts()[alertName];
        }
    }

    switch (action) {
    case NOTIFICATION: {
        if (!snarl->snore()->aplications().contains(app.name())) {
            snarl->snore()->registerApplication(app);
        }

        if (!alert.isActive()) {
            break;
        }
        sNotification = Notification(app, alert, title, text, icon, timeout);
        sNotification.data()->setSource(snarl);
        sNotification.hints().setPrivateValue(snarl, "clientSocket", client);
        break;
    }
    case ADD_CLASS:
        if (alertName.isEmpty()) {
            snoreDebug(SNORE_DEBUG) << "Error registering alert with empty name";
            break;
        }
        alert = Alert(alertName, icon);
        app.addAlert(alert);
        break;
    case REGISTER:
        if (!snarl->m_applications.contains(client)) {
            snarl->m_applications[client] = Application(appName, icon);
        } else {
            snoreDebug(SNORE_DEBUG) << appName << "already registred";
        }
        break;
    case UNREGISTER:
        snarl->snore()->deregisterApplication(app);
        snarl->m_applications.take(client);
        break;
    case ERROR:
    default:
        break;
    }
}

