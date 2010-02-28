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
    out+=QString("#?title="+title+"#?text="+text+"#?timeout="+QString::number(timeout));
    return out;
}

QDataStream & operator<< ( QDataStream & stream, const Notification & noti){
    stream<<noti.toSnalrString();
    return stream;
}

#include "notification.moc"
