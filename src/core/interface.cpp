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

#include "interface.h"
#include "snoreserver.h"

#include <QTimer>
#include <QDebug>

namespace Snore{

SnorePluginInfo::type SnorePluginInfo::typeFromString(const QString &t){
    if(t == QLatin1String("backend"))
        return BACKEND;
    if(t == QLatin1String("frontend"))
        return FRONTEND;
    return PLUGIN;
}

const QStringList &SnorePluginInfo::types(){
    static QStringList *list =NULL;
    if(list == NULL){
        list = new QStringList();
        *list<<"backend"<<"frontend"<<"plugin";
    }
    return *list;
}

SnorePlugin::SnorePlugin ( QString name ) :
    m_name ( name ),
    m_initialized(false)
{}

SnorePlugin::~SnorePlugin()
{
    qDebug()<<m_name<<this<<"deleted";
}

bool SnorePlugin::init( SnoreServer *snore )
{
    qDebug()<<"Initialize"<<m_name<<this<<snore;
    this->m_snore = snore;
    m_initialized = true;
    return true;
}

bool SnorePlugin::isInitialized(){
    return m_initialized;
}

SnoreServer* SnorePlugin::snore()
{
    return m_snore.data();
}

const QString &SnorePlugin::name() const
{
    return m_name;
}

void SnorePlugin::startTimeout(uint id,int timeout){
    if(timeout==-1)//sticky
        return;
    if(m_timeouts.contains(id)){
        QTimer *t = m_timeouts.take(id);
        t->stop();
        t->deleteLater();
        m_timeout_order.removeOne(id);
    }
    QTimer *timer= new QTimer(this);
    timer->setInterval(timeout*1000);
    timer->setSingleShot(true);
    m_timeouts.insert(id,timer);
    m_timeout_order.append(id);
    connect(timer,SIGNAL(timeout()),this,SLOT(notificationTimedOut()));
    timer->start();
}

void SnorePlugin::notificationTimedOut(){
    uint id = m_timeout_order.takeFirst();
    m_timeouts.take(id)->deleteLater();
    if(activeNotifications.contains(id)){
        Notification n = activeNotifications.take(id);
        snore()->closeNotification(n,NotificationEnums::CloseReasons::TIMED_OUT);
    }
}

Notification_Backend::Notification_Backend ( QString name ) :
    SnorePlugin ( name )
{

}

Notification_Backend::~Notification_Backend()
{
}

bool Notification_Backend::init( SnoreServer *snore )
{
    if(!SnorePlugin::init(snore))
        return false;
    connect( snore,SIGNAL( closeNotify( Snore::Notification ) ),this,SLOT( closeNotification( Snore::Notification) ) );
    connect( snore,SIGNAL( applicationInitialized( Snore::Application* ) ),this,SLOT( registerApplication( Snore::Application* ) ) );
    connect( snore,SIGNAL( applicationRemoved( Snore::Application* ) ),this,SLOT( unregisterApplication( Snore::Application* ) ) );
    if(!isPrimaryNotificationBackend())
        connect( snore,SIGNAL( notify(Snore::Notification) ),this,SLOT( notify( Snore::Notification ) ) );

    foreach(Application *a,snore->aplications()){
        this->registerApplication(a);
    }

    return true;

}

Notification_Frontend::Notification_Frontend ( QString name ) :
    SnorePlugin ( name )
{

}

Notification_Frontend::~Notification_Frontend()
{
}
bool Notification_Frontend::init( SnoreServer *snore )
{
    if(!SnorePlugin::init(snore))
        return false;
    connect( snore,SIGNAL ( closeNotify( Snore::Notification ) ),this,SLOT ( notificationClosed( Snore::Notification) ) );
    return true;
}
}
#include "interface.moc"
