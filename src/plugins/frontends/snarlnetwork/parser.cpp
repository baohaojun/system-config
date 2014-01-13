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



#include <QDir>
#include <QCryptographicHash>
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
    getSnpType.insert("type",TYPE);
    getSnpType.insert("app",APP);
    getSnpType.insert("version",VERSION);
    getSnpType.insert("action",ACTION);
    getSnpType.insert("register",REGISTER);
    getSnpType.insert("add_class",ADD_CLASS);
    getSnpType.insert("notification",NOTIFICATION);
    getSnpType.insert("unregister",UNREGISTER);
    getSnpType.insert("class",CLASS);
    getSnpType.insert("title",TITLE);
    getSnpType.insert("text",TEXT);
    getSnpType.insert("icon",ICON);
    getSnpType.insert("timeout",TIMEOUT);
}


SnarlNotification Parser::parse(QString &msg,QTcpSocket* client){
    SnarlNotification sNotification;
    sNotification.httpClient=false;
    sNotification.vailid=true;
    sNotification.clientSocket=client;
    sNotification.isNotification = false;


    snpTypes action(ERROR);
    if(msg.startsWith("GET ")){
        msg=msg.mid(msg.indexOf("/")+1);
        msg=msg.mid(0,msg.indexOf(" "));
        QByteArray dat(QByteArray::fromBase64(msg.toLatin1().data()));
        msg=QString(dat);
        qDebug()<<"Notification from a browser"<<msg;
        sNotification.httpClient=true;
    }

    QString appName;
    QString title;
    QString text;
    QString icon;
    QString alertName;
    int timeout=10;

    QString key;
    QString value;
    QStringList splitted=msg.split("#?");
    foreach(QString s,splitted){
        key = s.mid(0,s.indexOf("=")).toLower();
        value = s.mid(s.indexOf("=")+1);
        switch(getSnpType.value(key)){
        case APP:
            appName = value;
            break;
        case ACTION:
            action = getSnpType.value(value);
            sNotification.action = value;
            break;
        case  TITLE:
            title = value;
            break;
        case TEXT:
            text = value;
            break;
        case ICON:
            icon = value;
            break;
        case CLASS:
            alertName = value;
        case TIMEOUT:
            timeout = value.toInt();
            break;
        default:
            break;
        }
    }


    Application app;
    Alert alert;
    if(snarl->m_applications.contains(value))
    {
        app = snarl->m_applications[value];
    }
    else
    {
        app = Application(value, icon);
    }

    if(app.alerts().contains(alertName))
    {
        alert = app.alerts()[alertName];
    }
    else
    {
        if(title.isEmpty())
        {
            alert = Alert(alertName, alertName);
        }
        else
        {
            alert = Alert(alertName, title);
        }
    }

    sNotification.notification = Notification(app,alert,title,text,icon,timeout);
    qDebug() << sNotification.notification.title() << sNotification.notification.icon() << sNotification.notification.icon().isValid();
    sNotification.notification.setSource(snarl);


    switch(action)
    {
    case NOTIFICATION:
    {
        qDebug() << sNotification.notification.application();
        const Application &appl = sNotification.notification.application();
        if(!snarl->snore()->aplications().contains(appl.name()))
        {
            snarl->snore()->registerApplication(appl);
        }

        if(!sNotification.notification.alert().isActive())
        {
            break;
        }
        sNotification.isNotification = true;
        return sNotification;
        break;
    }
    case ADD_CLASS:
        if(!sNotification.notification.alert().isValid())
        {
            qDebug()<<"Error registering alert with empty name";
            break;
        }
        sNotification.notification.application().addAlert(sNotification.notification.alert());
        break;
    case REGISTER:
        if(sNotification.notification.application().isValid() && !snarl->m_applications.contains(sNotification.notification.application().name()))
        {
            snarl->m_applications.insert(sNotification.notification.application().name(), sNotification.notification.application());
        }
        else
        {
            qDebug()<<sNotification.notification.application()<<"already registred";
        }
        break;
    case UNREGISTER:
        snarl->snore()->deregisterApplication( sNotification.notification.application());
        break;
    case ERROR:
    default:
        sNotification.vailid=false;
        break;
    }
    sNotification.notification.hints().setValue("SnarlAction", sNotification.action);
    return sNotification;
}

