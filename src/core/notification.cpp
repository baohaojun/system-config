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

#include "notification.h"
#include <QDebug>
#include <QTcpSocket>
#include "snoreserver.h"



int Notification::DefaultTimeout=10;

Notification::Notification(uint id):source("none"),timeout(10),id(id),notification(true){}
Notification::Notification(QString source,QString title,QString text,QString icon,int timeout,uint id):source(source),title(title),text(text),timeout(timeout),id(id),icon(icon),notification(true)
{       
}

QString Notification::getIcon(){
    return icon;
}

bool Notification::isNotification(){
    return notification;
}

void Notification::setIsNotification(bool b){
    notification=b;
}
uint Notification::getID(){
    return id;
}

QString Notification::toSnalrString()const{
    QString out("type=SNP#?version=1.1");
    if(hints.contains("SNaction"))
        out+=QString("#?action="+hints.value("SNaction").value<QString>());
    if(!app.isEmpty())
        out+=QString("#?app="+app);
    if(!alert.isEmpty())
        out+=QString("#?class="+alert);
    if(hints.contains("SnarlIcon"))
        out+=QString("#?icon="+hints.value("SnarlIcon").value<QString>());
    out+=QString("#?title="+title+"#?text="+text+"#?timeout="+QString::number(timeout));
    return out;
}

QDataStream & operator<< ( QDataStream & stream, const Notification & noti){
    stream<<noti.toSnalrString();
    return stream;
}

#include "notification.moc"
