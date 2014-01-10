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

#include "trayicon.h"
#include "core/snore.h"

#include <QSystemTrayIcon>
#include <QMenu>
#include <QAction>

#include "core/version.h"

using namespace Snore;

TrayIcon::TrayIcon()        
{	
    _trayIcon = new QSystemTrayIcon(QIcon(":/root/snore.png"));
}

void TrayIcon::initConextMenu(SnoreCore *snore){
    _snore = snore;
    _trayIcon->setVisible(true);

    _trayMenu = new QMenu("SnoreNotify");
    _trayMenu->addAction(QString("SnoreNotify ").append(Version::version()));
    _trayMenu->addSeparator();
    connect(_trayMenu->addAction(QString("Test")), SIGNAL(triggered()), this, SLOT(slotTestNotification()));
    _trayMenu->addSeparator();
    foreach(const QString &back,_snore->notificationBackends()){
        QAction *b=  new QAction(back,this);
        connect(b,SIGNAL(triggered()),this,SLOT(setPrimaryBackend()));
        b->setCheckable(true);
        if(back == _snore->primaryNotificationBackend())
            b->setChecked(true);
        _backendActions.append(b);
        _trayMenu->addAction(b);
    }
    _trayMenu->addSeparator();
    _trayMenu->addAction("Exit",qApp,SLOT(quit()));


    _trayIcon->setContextMenu(_trayMenu);
}

void TrayIcon::hide(){
    _trayIcon->setVisible(false);
}

QSystemTrayIcon* TrayIcon::trayIcon(){
    return _trayIcon;
}

void TrayIcon::setPrimaryBackend(){
    QAction *a= dynamic_cast<QAction*>(sender());
    _snore->setPrimaryNotificationBackend(a->text());

    foreach(QAction *action,_backendActions){
        action->setChecked(false);
    }
    a->setChecked(true);

}

void TrayIcon::slotTestNotification()
{
    Notification n("SnoreNotify","Default","Hello World","This is Snore",Icon(":/root/snore.png"));
    _snore->broadcastNotification(n);
}

