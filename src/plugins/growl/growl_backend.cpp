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

#include "growl_backend.h"
#include <growl++.hpp>
#include <QtCore>

Q_EXPORT_PLUGIN2(growl_backend,Growl_Backend)

Growl_Backend::Growl_Backend(SnoreServer *snore):
Notification_Backend("Growl",snore),
id(0)
{
    const char *n[1] = { "SnoreNotification"};
    growl=new Growl(GROWL_TCP,NULL,"SnoreNotify",n,1);
}
Growl_Backend::~Growl_Backend(){
    delete growl;
}

int Growl_Backend::notify(QSharedPointer<Notification> notification){
    QString title=Notification::toPlainText(notification->title());
    QString text=Notification::toPlainText(notification->text());
    qDebug()<<title<<text;
    growl->Notify("SnoreNotification",title.toLatin1().data(),text.toLatin1().data(),NULL,notification->icon().toLatin1().data());
    return ++id;
}

void Growl_Backend::closeNotification(int nr){

}

#include "growl_backend.moc"
