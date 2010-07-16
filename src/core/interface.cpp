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

#include "interface.h"
#include "snoreserver.h"

SnorePlugin::SnorePlugin(QString name,SnoreServer *snore):
        _name(name),
        _snore(snore)
{}

SnorePlugin::~SnorePlugin(){
    delete _snore;
}

void SnorePlugin::setSnore(SnoreServer *snore){
    this->_snore=snore;
}

SnoreServer* SnorePlugin::snore(){
    return _snore;
}

const QString &SnorePlugin::name() const{
    return _name;
}

Notification_Backend::Notification_Backend(QString name, SnoreServer *snore):
        SnorePlugin(name,snore)
{

}

Notification_Backend::~Notification_Backend()
{
}

Notification_Frontend::Notification_Frontend(QString name, SnoreServer *snore):
        SnorePlugin(name,snore)
{

}

Notification_Frontend::~Notification_Frontend(){
}

#include "interface.moc"
