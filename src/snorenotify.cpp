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
    m_settings("SnoreNotify","SnoreNotify")
{
    qApp->setApplicationName("SnoreNotify");
    qApp->setOrganizationName("SnoreNotify");
    m_trayIcon = new TrayIcon();
    m_snore = new SnoreCore(m_trayIcon->trayIcon());
    m_snore->loadPlugins(SnorePlugin::ALL);
    load();
    m_trayIcon->initConextMenu(m_snore);

    connect(qApp,SIGNAL(aboutToQuit()),this,SLOT(exit()));
    snoreDebug( SNORE_DEBUG ) << "Snorenotfiy initialized with" << m_snore->primaryNotificationBackend();
}

SnoreNotify::~SnoreNotify(){
    delete m_snore;
    delete m_trayIcon;
}

void SnoreNotify::load()
{
    QString backend = m_settings.value("notificationBackend").toString();
    if(!backend.isEmpty())
    {
        if(!m_snore->setPrimaryNotificationBackend(backend))
        {
            m_snore->setPrimaryNotificationBackend();
        }
    }
    else
    {
        m_snore->setPrimaryNotificationBackend();
    }
}
void SnoreNotify::save(){
    m_settings.setValue("notificationBackend",m_snore->primaryNotificationBackend());
}

void SnoreNotify::exit(){
    snoreDebug( SNORE_DEBUG )<<"Saving snore settings";
    foreach(const Application &a,m_snore->aplications())
    {
        m_snore->deregisterApplication(a);
    }
    save();
    m_trayIcon->hide();
}

