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
namespace Snore{

SnorePlugin::SnorePlugin ( QString name,SnoreServer *snore ) :
        _name ( name ),
        _snore ( snore )
{}

SnorePlugin::~SnorePlugin()
{
    delete _snore;
}

void SnorePlugin::setSnore ( SnoreServer *snore )
{
    this->_snore=snore;
}

SnoreServer* SnorePlugin::snore()
{
    return _snore;
}

const QString &SnorePlugin::name() const
{
    return _name;
}

void SnorePlugin::startTimeout(uint id,int timeout){
    if(timeout==-1)//sticky
        return;
    if(timeouts.contains(id)){
        QTimer *t = timeouts.take(id);
        t->stop();
        t->deleteLater();
        timeout_order.removeOne(id);
    }
    QTimer *timer= new QTimer(this);
    timer->setInterval(timeout*1000);
    timer->setSingleShot(true);
    timeouts.insert(id,timer);
    timeout_order.append(id);
    connect(timer,SIGNAL(timeout()),this,SLOT(notificationTimedOut()));
    timer->start();
}

void SnorePlugin::notificationTimedOut(){
    uint id = timeout_order.takeFirst();
    timeouts.take(id)->deleteLater();
    if(activeNotifications.contains(id)){
        Notification n = activeNotifications.take(id);
        snore()->closeNotification(n,NotificationEnums::CloseReasons::TIMED_OUT);
    }
}

Notification_Backend::Notification_Backend ( QString name, SnoreServer *snore ) :
        SnorePlugin ( name,snore )
{

}

Notification_Backend::~Notification_Backend()
{
}

Notification_Frontend::Notification_Frontend ( QString name, SnoreServer *snore ) :
        SnorePlugin ( name,snore )
{

}

Notification_Frontend::~Notification_Frontend()
{
}

}
#include "interface.moc"
