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


#define SNORENOTIFIER_MESSAGE_ID  WM_USER + 238

using namespace Snarl::V42;

Q_EXPORT_PLUGIN2(snarl_backend,Snarl_Backend);

Snarl_Backend::Snarl_Backend(SnoreServer *snore):
Notification_Backend("SnarlBackend",snore)
{
	winIDWidget = new SnarlWidget(this);
	SnarlInterface *snarlInterface = new SnarlInterface();
	_applications.insert("SnoreNotify",snarlInterface);
	qDebug()<<"Initiating Snarl Backend, Snarl version: "<<snarlInterface->GetVersion();
	_defautSnarlinetrface = new SnarlInterface();


}

Snarl_Backend::~Snarl_Backend()
{
	foreach(Application *a,this->snore()->aplications().values()){
		unregisterApplication(a);
	}
	delete _defautSnarlinetrface;
}

void Snarl_Backend::registerApplication(Application *application){
	SnarlInterface *snarlInterface = NULL;
	if(_applications.contains(application->name())){
		snarlInterface = _applications.value(application->name());
	}else{
		snarlInterface = new SnarlInterface();
		_applications.insert(application->name(),snarlInterface);
	}
	qDebug()<<"Register with Snarl"<<application->name()<<application->icon();
	snarlInterface->Register(application->name().toUtf8().constData(),
		application->name().toUtf8().constData(),
		application->icon().toUtf8().constData(),
		0,winIDWidget->winId(),SNORENOTIFIER_MESSAGE_ID);

	foreach(Alert *alert,application->alerts()){
		snarlInterface->AddClass(application->name().toUtf8().constData(),
			alert->name().toUtf8().constData());
	}
}

void Snarl_Backend::unregisterApplication(Application *application){
	SnarlInterface *snarlInterface = _applications.take(application->name());
	if(snarlInterface == NULL)
		return;
	snarlInterface->Unregister(application->name().toUtf8().constData());
	delete snarlInterface;
}

int Snarl_Backend::notify(Notification notification){
	SnarlInterface *snarlInterface = _applications.value(notification.application());
	qDebug()<<notification.application();
	if(snarlInterface == NULL){
		qDebug()<<notification.application()<<"not in snarl interfaces, defaulting";
		qDebug()<<_applications.keys();
		snarlInterface = _defautSnarlinetrface;
	}
	uint id = notification.id();
	if(id == 0){
		id = snarlInterface->Notify(notification.alert().toUtf8().constData(),
			Notification::toPlainText(notification.title()).toUtf8().constData(),
			Notification::toPlainText(notification.text()).toUtf8().constData(),
			notification.timeout(),
			notification.icon().toUtf8().constData());
			
		foreach(const Action *a, notification.actions()){
			qDebug()<<"snarl add action"<<a->id<<a->name;
			snarlInterface->AddAction(id,a->name.toUtf8().constData(),QString("@").append(QString::number(a->id)).toUtf8().constData());
		}
		//add ack stuff
		activeNotifications[id] = notification;
	}else{
		//update message
		snarlInterface->Update(notification.id(),
			notification.alert().toUtf8().constData(),
			Notification::toPlainText(notification.title()).toUtf8().constData(),
			Notification::toPlainText(notification.text()).toUtf8().constData(),
			notification.timeout(),
			notification.icon().toUtf8().constData());
	}
	return id;
}

void Snarl_Backend::closeNotification(Notification notification){
	_defautSnarlinetrface->Hide(notification.id());
	activeNotifications.remove(notification.id());
}

bool Snarl_Backend::isPrimaryNotificationBackend(){
	return true;
}

SnarlWidget::SnarlWidget(Snarl_Backend * snarl):
_snarl(snarl)
{
	SNARL_GLOBAL_MESSAGE = SnarlInterface::Broadcast();
}

bool SnarlWidget::winEvent(MSG * msg, long * result){
	if(msg->message == SNARL_GLOBAL_MESSAGE){
		int action = msg->wParam;
		if(action == SnarlEnums::SnarlLaunched){
			foreach(Application *a,_snarl->snore()->aplications()){
				_snarl->registerApplication(a);
			}
		}

	}else if(msg->message == SNORENOTIFIER_MESSAGE_ID){
		int action = msg->wParam & 0xffff;
		int data = (msg->wParam & 0xffffffff) >> 16;
		uint notificationID = msg->lParam;
		qDebug()<<_snarl->activeNotifications.keys();
		Notification notification(_snarl->activeNotifications[notificationID]);
		qDebug()<<"arg"<<notification.toString();
			qDebug()<<notification.id();
		qDebug()<<"recived a Snarl callback id:"<<notificationID<<"action:"<<action;
		qDebug()<<"data:"<<data;
		Notification::closeReasons reason = Notification::NONE;
		switch(action){
		case SnarlEnums::CallbackInvoked:
			reason = Notification::CLOSED;
			break;
		case SnarlEnums::CallbackMenuSelected:
			reason = Notification::CLOSED;
			notification.setActionInvoked(data);
			_snarl->snore()->notificationActionInvoked(notification);
			break;
		case SnarlEnums::CallbackClosed:
			 reason = Notification::DISMISSED; 
			break;
		case SnarlEnums::CallbackTimedOut:
			reason = Notification::TIMED_OUT; 
			break;
		default:
			qDebug()<<"Unknown snarl action found!!";
			return false;
		}
		_snarl->snore()->closeNotification(notification,reason);
		return true;
	}
	return false;
}



#include "snarl_backend.moc"
