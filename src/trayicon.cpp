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

#include "trayicon.h"
#include "core/snoreserver.h"

#include <QSystemTrayIcon>
#include <QMenu>
#include <QAction>

TrayIcon::TrayIcon(QSystemTrayIcon *trayIcon, SnoreServer *snore ):
        _trayIcon(trayIcon),
        _snore(snore)
{
}

void TrayIcon::initConextMenu(){

    _trayMenu = new QMenu("SnoreNotify");
    _trayMenu->addAction("SnoreNotify");
    _trayMenu->addSeparator();
    foreach(Notification_Backend *back,_snore->primaryNotificationBackends()){
        QAction *b=  new QAction(back->name(),this);
        connect(b,SIGNAL(triggered()),this,SLOT(setPrimaryBackend()));
        _trayMenu->addAction(b);
    }
    _trayMenu->addSeparator();
    _trayMenu->addAction("Exit",qApp,SLOT(quit()));


    _trayIcon->setContextMenu(_trayMenu);
}

void TrayIcon::setPrimaryBackend(){
    QAction *a= dynamic_cast<QAction*>(sender());
    _snore->setPrimaryNotificationBackend(_snore->primaryNotificationBackends().value(a->text()));
}



#include "trayicon.moc"
