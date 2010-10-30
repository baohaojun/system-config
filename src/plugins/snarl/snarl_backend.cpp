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

#include "core/snoreserver.h"

#include <QtCore>
#include <QTextEdit>

#include <iostream>

Q_EXPORT_PLUGIN2(snarl_backend,Snarl_Backend)

QAbstractEventDispatcher::EventFilter Snarl_Backend::originalEventFilter = NULL;

Snarl_Backend::Snarl_Backend(SnoreServer *snore):
        Notification_Backend("SnarlBackend",snore)
{
    Snarl::SnarlInterface *snarlInterface = new Snarl::SnarlInterface();
    _applications.insert("SnoreNotify",snarlInterface);
    qDebug()<<"Initiating Snarl Backend, Snarl version: "<<snarlInterface->GetVersionExA();
    _defautSnarlinetrface = new Snarl::SnarlInterface();

    //originalEventFilter = QAbstractEventDispatcher::instance()->setEventFilter(&eventFilter);
}

Snarl_Backend::~Snarl_Backend()
{
    foreach(Application *a,this->snore()->aplications().values()){
        unregisterApplication(a);
    }
    delete _defautSnarlinetrface;
    QAbstractEventDispatcher::instance()->setEventFilter(originalEventFilter);
}

void Snarl_Backend::registerApplication(Application *application){
    Snarl::SnarlInterface *snarlInterface = new Snarl::SnarlInterface();
    _applications.insert(application->name(),snarlInterface);

    const char *appName = strdup(application->name().toAscii().constData());
    const char *icon = strdup(application->icon().toAscii().constData());
    snarlInterface->RegisterApp(appName,icon,icon);

    foreach(Alert *alert,application->alerts()){
        snarlInterface->RegisterAlert(appName,alert->name().toAscii().constData());
    }
    delete [] appName;
    delete [] icon;
}

void Snarl_Backend::unregisterApplication(Application *application){
    Snarl::SnarlInterface *snarlInterface = _applications.take(application->name());
    if(snarlInterface == NULL)
        return;
    snarlInterface->UnregisterApp();
    delete snarlInterface;
}

int Snarl_Backend::notify(QSharedPointer<Notification>notification){
    Snarl::SnarlInterface *snarlInterface = _applications.value(notification->application());
    qDebug()<<notification->application();
    if(snarlInterface == NULL)
        snarlInterface = _defautSnarlinetrface;

    int id = notification->id();
    const char *title = strdup(Notification::toPlainText(notification->title()).toAscii().constData());
    const char *text =  strdup(Notification::toPlainText(notification->text()).toAscii().constData());
    const char *icon =  strdup(notification->icon().toAscii().constData());

    if(notification->id()==0){
        printf("Calling SnarlMessage\n"
               "Title: \"%s\"\n"
               "Text: \"%s\"\n"
               "Timeout: \"%i\"\n"
               "Icon: \"%s\"\n",title,text,notification->timeout(),icon);
        id = snarlInterface->ShowMessage(title,text,notification->timeout(), icon);
    }else{
        //update message
        printf("Updating SnarlMessage ID: \"%i\"\n"
               "Title: \"%s\"\n"
               "Text: \"%s\"\n"
               "Icon: \"%s\"\n",notification->id(),title,text,icon);
        snarlInterface->UpdateMessage(notification->id(),title, text,icon);
    }

    delete[] title;
    delete[] text;
    delete[] icon;
    return id;
}

void Snarl_Backend::closeNotification(QSharedPointer<Notification> notification){
    _defautSnarlinetrface->HideMessage(notification->id());
}

bool Snarl_Backend::eventFilter(void *message){
    MSG *msg;
    msg = (MSG*)message;
    if(msg!=NULL){

//        uint id = static_cast<LONG32>(reinterpret_cast<DWORD_PTR>(msg->hwnd));
//        qDebug()<<QString::number(id);
    }
    return originalEventFilter==NULL?true:originalEventFilter(message);
}

bool Snarl_Backend::isPrimaryNotificationBackend(){
    return true;
}

#include "snarl_backend.moc"
