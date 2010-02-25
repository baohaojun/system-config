#include "snarl_backend.h"
#include <QtCore>
#include <QTextEdit>
#include <iostream>




Q_EXPORT_PLUGIN2(snarl_backend,Snarl_Backend)

Snarl_Backend::Snarl_Backend()
{
    setProperty("name","Snarl_Backend");
    snarlInterface=new Snarl::SnarlInterface();
    qDebug()<<"Initiating Snarl Backend, Snarl version: "<<snarlInterface->GetVersionExA();
     this->installEventFilter(this);

}


int Snarl_Backend::notify(QSharedPointer<Notification>notification){
    int timeout=notification->timeout>=0?notification->timeout/1000:10;
    if(notification->id==0){
        QString title=Notification::toPlainText(notification->title);
        QString text=Notification::toPlainText(notification->text);
        qDebug()<<"Calling Snarl"<<title<< text<<QString::number(timeout)<< notification->getIcon();
        return snarlInterface->ShowMessage(title.toLocal8Bit().data(), text.toLocal8Bit().data(),timeout, notification->getIcon().toLocal8Bit().data());
    }else{
        //update message
        snarlInterface->UpdateMessage(LONG32(notification->id),notification->title.toLocal8Bit().data(), notification->text.toLocal8Bit().data(),notification->getIcon().toLocal8Bit().data());
        return notification->id;
    }
}

void Snarl_Backend::closeNotification(int nr){
    snarlInterface->HideMessage(nr);
}
bool Snarl_Backend::eventFilter(QObject *obj, QEvent *event){
    qDebug()<<obj->objectName();
    return true;
}


#include "snarl_backend.moc"
