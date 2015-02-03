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

#include <iostream>
#include <stdlib.h>

#include <QApplication>

using namespace Snore;

SnoreNotify::SnoreNotify()
{
    m_trayIcon = new TrayIcon();
    m_snore = new SnoreCore();
    m_snore->loadPlugins(SnorePlugin::ALL);
    m_trayIcon->initConextMenu(m_snore);

    snoreDebug(SNORE_DEBUG) << "Snorenotfiy initialized with" << m_snore->primaryNotificationBackend();
}

SnoreNotify::~SnoreNotify()
{
    delete m_snore;
    delete m_trayIcon;
}

