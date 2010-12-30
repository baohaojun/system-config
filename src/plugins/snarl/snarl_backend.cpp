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
#include <QtDebug>

#include <iostream>

//disable some msvc warnings
#define _CRT_SECURE_NO_WARNINGS

using namespace Snarl::V41;

Q_EXPORT_PLUGIN2(snarl_backend,Snarl_Backend)

QAbstractEventDispatcher::EventFilter Snarl_Backend::originalEventFilter = NULL;

Snarl_Backend::Snarl_Backend(SnoreServer *snore):
        Notification_Backend("SnarlBackend",snore)
{
    SnarlInterface *snarlInterface = new SnarlInterface();
    _applications.insert("SnoreNotify",snarlInterface);
	qDebug()<<"Initiating Snarl Backend, Snarl version: "<<snarlInterface->GetVersion();
    _defautSnarlinetrface = new SnarlInterface();

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
    SnarlInterface *snarlInterface = new SnarlInterface();
    _applications.insert(application->name(),snarlInterface);

    const char *appName = strdup(application->name().toUtf8().constData());
    const char *icon = strdup(application->icon().toUtf8().constData());
    snarlInterface->RegisterApp(appName,icon,icon);

    foreach(Alert *alert,application->alerts()){
		snarlInterface->AddClass(appName,alert->name().toUtf8().constData());
    }
    delete [] appName;
    delete [] icon;
}

void Snarl_Backend::unregisterApplication(Application *application){
    SnarlInterface *snarlInterface = _applications.take(application->name());
    if(snarlInterface == NULL)
        return;
    snarlInterface->UnregisterApp();
    delete snarlInterface;
}

int Snarl_Backend::notify(QSharedPointer<Notification>notification){
    SnarlInterface *snarlInterface = _applications.value(notification->application());
    qDebug()<<notification->application();
    if(snarlInterface == NULL)
        snarlInterface = _defautSnarlinetrface;

    int id = notification->id();
	const char *alert = strdup(notification->alert().toUtf8().constData());
    const char *title = strdup(Notification::toPlainText(notification->title()).toUtf8().constData());
    const char *text =  strdup(Notification::toPlainText(notification->text()).toUtf8().constData());
    const char *icon =  strdup(notification->icon().toUtf8().constData());

	qDebug()<<"Calling SnarlMessage:"<<notification->id()<<"Title:"<<title<<"Text:"<<text<<"Timeout:"<<QString::number(notification->timeout())<<"Icon:"<<icon;
    if(notification->id()==0){
		id = snarlInterface->EZNotify(alert,title,text,notification->timeout(), icon);
    }else{
        //update message
		snarlInterface->EZUpdate(notification->id(),title, text,notification->timeout(),icon);
    }

	delete[] alert;
    delete[] title;
    delete[] text;
    delete[] icon;
    return id;
}

void Snarl_Backend::closeNotification(QSharedPointer<Notification> notification){
	_defautSnarlinetrface->Hide(notification->id());
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
