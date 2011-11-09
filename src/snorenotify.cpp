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

#include "snorenotify.h"
#include "trayicon.h"
#include "core/snoreserver.h"

#include <QDir>
#include <QFile>
#include <QList>
#include <QDebug>
#include <QPluginLoader>
#include <QSystemTrayIcon>
#include <QSettings>

#include <iostream>

using namespace Snore;

SnoreNotify::SnoreNotify():
    _settings("TheOneRing","SnoreNotify")
{
    _trayIcon = new TrayIcon();
    _snore = new Snore::SnoreServer(_trayIcon->trayIcon());

    QDir pluginsDir ( qApp->applicationDirPath() +"/snoreplugins" );
    foreach ( QString fileName, pluginsDir.entryList ( QDir::Files ) )
    {
        _snore->publicatePlugin ( pluginsDir.absoluteFilePath ( fileName ) );
    }


    load();

    _trayIcon->initConextMenu(_snore);

    connect(qApp,SIGNAL(aboutToQuit()),this,SLOT(exit()));
}

SnoreNotify::~SnoreNotify(){
    delete _snore;
    delete _trayIcon;
}

void SnoreNotify::load(){
    _snore->setPrimaryNotificationBackend(_settings.value("notificationBackend").toString());
}

void SnoreNotify::save(){
    _settings.setValue("notificationBackend",_snore->primaryNotificationBackend());
}

void SnoreNotify::exit(){
    qDebug()<<"Saving snore settings";
    foreach(Application *a,_snore->aplications()){
        _snore->removeApplication(a->name());
    }
    save();
    _trayIcon->hide();
}


#include "snorenotify.moc"
