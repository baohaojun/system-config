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

#include "growl.h"
#include "gntp.h"

#include "core/snore.h"


#include <QtCore>

using namespace Snore;

Q_EXPORT_PLUGIN2(growl,Growl)

Growl *Growl::s_instance = NULL;

Growl::Growl():
    SnoreBackend("Growl"),
    m_id(0)
{
    s_instance = this;
}

Growl::~Growl(){
    if(snore() != NULL){
        foreach(Application *a,snore()->aplications()){
            this->unregisterApplication(a);
        }
    }
}

void Growl::registerApplication(Application *application){
    gntp *growl = new gntp(application->name().toUtf8().constData(),application->icon().localUrl().toUtf8().constData());


    gntp::gntp_callback callback(&Growl::gntpCallback);
    growl->set_gntp_callback(callback);
    std::vector<std::string> alerts;
    foreach(Alert *a,application->alerts()){
        alerts.push_back(a->name().toUtf8().constData());
    }

    try{
        growl->regist(alerts);
    }catch(const std::exception& e){
        qDebug()<<"Growl:"<<e.what();
    }
    m_applications.insert(application->name(),growl);
}

void Growl::unregisterApplication(Application *application){
    gntp *growl = m_applications.take(application->name());
    if(growl == NULL)
        return;
    delete growl;
}

uint Growl::notify(Notification notification){
    gntp *growl = m_applications.value(notification.application());
    if(growl == NULL)
        return -1;
    //qDebug()<<"Notify Growl:"<<notification.application()<<Notification.toPlainText(notification.title());
    try{
        growl->notify(notification.alert().toUtf8().constData(),m_id,
                      Notification::toPlainText(notification.title()).toUtf8().constData(),
                      Notification::toPlainText(notification.text()).toUtf8().constData(),
                      notification.icon().localUrl().isEmpty()?NULL:notification.icon().localUrl().toUtf8().constData(),NULL,"1");

    }catch(const std::exception& e){
        qDebug()<<"Growl:"<<e.what();
    }
    return m_id++;
}

void Growl::closeNotification(Notification notification){
    Q_UNUSED(notification);
}

void Growl::gntpCallback(const int &id,const std::string &reason,const std::string &data){
    qDebug()<<"Growl Callback"<<id<<QString(reason.c_str())<<QString(data.c_str());
    Notification n = s_instance->snore()->getActiveNotificationByID(id);
    NotificationEnums::CloseReasons::closeReasons r = NotificationEnums::CloseReasons::NONE;
    if(reason == "TIMEDOUT")
        r = NotificationEnums::CloseReasons::TIMED_OUT;
    else if(reason == "CLOSED")
        r = NotificationEnums::CloseReasons::DISMISSED;
    else if(reason == "CLICK"){
        r = NotificationEnums::CloseReasons::CLOSED;
        n.setActionInvoked(QString(data.c_str()).toInt());
        s_instance->snore()->notificationActionInvoked(n);
    }
    s_instance->snore()->closeNotification(n,r);
}



#include "growl.moc"
