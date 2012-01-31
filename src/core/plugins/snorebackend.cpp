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

#include "snorebackend.h"
#include "../snore.h"
#include "../application.h"

#include <QTimer>
#include <QPluginLoader>
#include <QDir>
#include <QDebug>

namespace Snore{

SnoreBackend::SnoreBackend ( const QString &name ) :
    SnorePlugin ( name )
{

}

SnoreBackend::~SnoreBackend()
{
    qDebug()<<"Deleting"<<name();
}


bool SnoreBackend::init( SnoreCore *snore )
{
    if(!SnorePlugin::init(snore))
        return false;
    connect( snore,SIGNAL( closeNotify( Snore::Notification ) ),this,SLOT( closeNotification( Snore::Notification) ) );
    connect( snore,SIGNAL( applicationInitialized( Snore::Application* ) ),this,SLOT( registerApplication( Snore::Application* ) ) );
    connect( snore,SIGNAL( applicationRemoved( Snore::Application* ) ),this,SLOT( unregisterApplication( Snore::Application* ) ) );

    foreach(Application *a,snore->aplications()){
        this->registerApplication(a);
    }

    return true;
}

SnoreSecondaryBackend::SnoreSecondaryBackend(const QString &name)
    :SnoreBackend(name)
{

}

SnoreSecondaryBackend::~SnoreSecondaryBackend()
{
    qDebug()<<"Deleting"<<name();
}

bool SnoreSecondaryBackend::init(SnoreCore *snore)
{
    connect( snore,SIGNAL( notify(SnoreCore::Notification) ),this,SLOT( notify( SnoreCore::Notification ) ) );
    return SnoreBackend::init(snore);
}

}
#include "snorebackend.moc"
