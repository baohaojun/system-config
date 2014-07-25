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

#include "growl.h"
#include "gntp.h"

#include "core/snore.h"
#include "core/snore_p.h"


#include <QtCore>
#include <QTcpSocket>

using namespace Snore;

Q_EXPORT_PLUGIN2(libsnore_backend_growl,Growl)

Growl *Growl::s_instance = NULL;

Growl::Growl():
    SnoreBackend("Growl",false,false),
    m_id(0)
{
    s_instance = this;
}

Growl::~Growl()
{
}

bool Growl::initialize(SnoreCore *snore)
{

    QTcpSocket qsocket;
    qsocket.connectToHost("localhost", 23053);
    if(qsocket.waitForConnected(100))
    {
        qsocket.write(QString("GNTP/1.0\r\n").toUtf8());
        if(qsocket.waitForReadyRead(100))
        {
            snoreDebug( SNORE_DEBUG ) << QString::fromUtf8(qsocket.readAll());
            return SnoreBackend::initialize(snore);
        }
    }
    snoreDebug( SNORE_DEBUG ) << "Growl is not running";
    return false;

}

void Growl::slotRegisterApplication(const Application &application)
{
    gntp *growl = new gntp(application.name().toUtf8().constData(),application.icon().localUrl().toUtf8().constData());

    gntp::gntp_callback callback(&Growl::gntpCallback);
    growl->set_gntp_callback(callback);

    //    snoreDebug( SNORE_DEBUG ) << application.name().toUtf8().constData();
    std::vector<std::string> alerts;
    foreach(const Alert &a,application.alerts())
    {
        snoreDebug( SNORE_DEBUG ) << a.name().toUtf8().constData();
        alerts.push_back(a.name().toUtf8().constData());
    }

    try
    {
        growl->regist(alerts);
    }catch(const std::exception& e)
    {
        snoreDebug( SNORE_WARNING ) << e.what();
    }
    m_applications.insert(application.name(),growl);
}

void Growl::slotDeregisterApplication(const Application &application)
{
    gntp *growl = m_applications.take(application.name());
    if(growl == NULL)
    {
        return;
    }
    delete growl;
}

void Growl::slotNotify(Notification notification)
{
    gntp *growl = m_applications.value(notification.application().name());
    QString alert = notification.alert().name();
    snoreDebug( SNORE_DEBUG ) << "Notify Growl:" <<notification.application() << alert << Snore::toPlainText(notification.title());
    try
    {
        growl->notify(alert.toUtf8().constData(),notification.id(),
                      Snore::toPlainText(notification.title()).toUtf8().constData(),
                      Snore::toPlainText(notification.text()).toUtf8().constData(),
                      notification.icon().localUrl().isEmpty()?NULL:notification.icon().localUrl().toUtf8().constData(),NULL,"1");

    }
    catch(const std::exception& e)
    {
        snoreDebug( SNORE_WARNING ) << e.what();
    }
    startTimeout(notification);
}

void Growl::gntpCallback(const int &id,const std::string &reason,const std::string &data)
{
    //    snoreDebug( SNORE_DEBUG ) << id << QString(reason.c_str()) << QString(data.c_str());
    Notification n = s_instance->snore()->getActiveNotificationByID(id);
    Notification::CloseReasons r = Notification::NONE;
    if(reason == "TIMEDOUT")
    {
        r = Notification::TIMED_OUT;
    }
    else if(reason == "CLOSED")
    {
        r = Notification::DISMISSED;
    }
    else if(reason == "CLICK")
    {
        r = Notification::CLOSED;
        s_instance->snore()->d()->notificationActionInvoked(n);
    }
    s_instance->closeNotification(n,r);
}

