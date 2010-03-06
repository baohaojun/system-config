#include "growl_backend.h"
#include <growl++.hpp>
#include <QtCore>

Q_EXPORT_PLUGIN2(growl_backend,Growl_Backend)
        Growl_Backend::Growl_Backend():id(0)
{
    setProperty("name","Growl");
    const char *n[1] = { "SnoreNotification"};
    growl=new Growl(GROWL_TCP,NULL,"SnoreNotify",n,1);
}
Growl_Backend::~Growl_Backend(){
    delete growl;
}

int Growl_Backend::notify(QSharedPointer<Notification>notification){
    QString title=Notification::toPlainText(notification->title);
    QString text=Notification::toPlainText(notification->text);
    qDebug()<<title<<text;
    growl->Notify("SnoreNotification",title.toLatin1().data(),text.toLatin1().data(),NULL,notification->getIcon().toLatin1().data());
return ++id;
}

void Growl_Backend::closeNotification(int nr){

}

#include "growl_backend.moc"
