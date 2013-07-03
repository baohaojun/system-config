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

#include "snorenotify.h"
#include "trayicon.h"
#include "core/snore.h"

#include <QDir>
#include <QFile>
#include <QList>
#include <QDebug>
#include <QPluginLoader>
#include <QSystemTrayIcon>
#include <QSettings>

#include <iostream>
#include <stdlib.h>

using namespace Snore;

SnoreNotify::SnoreNotify():
    m_settings("TheOneRing","SnoreNotify")
{
    qApp->setApplicationName("SnoreNotify");
    qApp->setOrganizationName("TheOneRing");
    m_trayIcon = new TrayIcon();
    m_snore = new SnoreCore(m_trayIcon->trayIcon());
    m_snore->loadPlugins(PluginContainer::ALL);
    load();
    m_trayIcon->initConextMenu(m_snore);

    connect(qApp,SIGNAL(aboutToQuit()),this,SLOT(exit()));
}

SnoreNotify::~SnoreNotify(){
    delete m_snore;
    delete m_trayIcon;
}

void SnoreNotify::load(){
    m_snore->setPrimaryNotificationBackend(m_settings.value("notificationBackend","SystemTray").toString());
}

void SnoreNotify::save(){
    m_settings.setValue("notificationBackend",m_snore->primaryNotificationBackend());
}

void SnoreNotify::exit(){
    qDebug()<<"Saving snore settings";
    foreach(Application *a,m_snore->aplications()){
        m_snore->removeApplication(a->name());
    }
    save();
    m_trayIcon->hide();
}


#include "snorenotify.moc"
