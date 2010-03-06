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
Snarl_Backend::~Snarl_Backend(){
    delete snarlInterface;
}

int Snarl_Backend::notify(QSharedPointer<Notification>notification){
    int timeout=notification->timeout>=0?notification->timeout:10;
    if(notification->getID()==0){
        QString title=Notification::toPlainText(notification->title);
        QString text=Notification::toPlainText(notification->text);
        std::cout<<"Calling Snarl"<<title.toLocal8Bit().data()<< text.toLocal8Bit().data()<<" "<<QString::number(timeout).toLatin1().data()<< notification->getIcon().toLocal8Bit().data()<<std::endl;
        return snarlInterface->ShowMessage(title.toLocal8Bit().data(), text.toLocal8Bit().data(),timeout, notification->getIcon().toLocal8Bit().data());
    }else{
        //update message
        snarlInterface->UpdateMessage(LONG32(notification->getID()),notification->title.toLocal8Bit().data(), notification->text.toLocal8Bit().data(),notification->getIcon().toLocal8Bit().data());
        return notification->getID();
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
