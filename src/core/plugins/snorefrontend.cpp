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

#include "snorefrontend.h"
#include "../snore.h"

#include <QTimer>
#include <QPluginLoader>
#include <QDir>
#include <QDebug>

namespace Snore{

SnoreFrontend::SnoreFrontend ( const QString &name ) :
    SnorePlugin ( name )
{

}

SnoreFrontend::~SnoreFrontend()
{
    qDebug()<<"Deleting"<<name();
}

bool SnoreFrontend::init( SnoreCore *snore )
{
    if(!SnorePlugin::init(snore))
        return false;
    connect( snore,SIGNAL ( notificationClosed( Snore::Notification ) ),this,SLOT ( notificationClosed( Snore::Notification) ) );
    return true;
}
}
