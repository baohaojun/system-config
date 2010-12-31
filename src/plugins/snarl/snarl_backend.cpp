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


#define SNORENOTIFIER_MESSAGE_ID 0x4532

using namespace Snarl::V41;

Q_EXPORT_PLUGIN2(snarl_backend,Snarl_Backend);

Snarl_Backend::Snarl_Backend(SnoreServer *snore):
Notification_Backend("SnarlBackend",snore)
{
	activeNotifications = new QHash<int,QSharedPointer<Notification> > ;
	winIDWidget = new SnarlWidget(this);
	SnarlInterface *snarlInterface = new SnarlInterface();
	qDebug()<<"WinID:"<<winIDWidget->winId();
	snarlInterface->RegisterApp("SnoreNotify","SnoreNotify",NULL,winIDWidget->winId(),SNORENOTIFIER_MESSAGE_ID);
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
	delete activeNotifications;
}

void Snarl_Backend::registerApplication(Application *application){
	SnarlInterface *snarlInterface = new SnarlInterface();
	_applications.insert(application->name(),snarlInterface);

	const char *appName = strdup(application->name().toUtf8().constData());
	const char *icon = strdup(application->icon().toUtf8().constData());
	snarlInterface->RegisterApp(appName,appName,icon,winIDWidget->winId(),SNORENOTIFIER_MESSAGE_ID);

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
		activeNotifications->insert(id,notification);
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
	activeNotifications->remove(notification->id());
}

bool Snarl_Backend::isPrimaryNotificationBackend(){
	return true;
}

SnarlWidget::SnarlWidget(Snarl_Backend * snarl):
_snarl(snarl)
{

}
bool SnarlWidget::winEvent(MSG * msg, long * result){
	qDebug()<<"Event loop"<<msg->message<<SNORENOTIFIER_MESSAGE_ID;
	if(msg->message == SNORENOTIFIER_MESSAGE_ID){
		int action = msg->wParam;
		int notificationID = msg->lParam;
		QSharedPointer<Notification> notification = _snarl->activeNotifications->value(notificationID);
		qDebug()<<"recived a Snarl callback id:"<<notificationID<<"action:"<<action;
		switch(action){
		case SnarlEnums::NotificationAck:
			notification->setActionInvoked(Notification::ACTION_1); 
			break;
		case SnarlEnums::NotificationClicked:
			notification->setActionInvoked(Notification::ACTION_2); 
			break;
		case SnarlEnums::NotificationMiddleButton:
			notification->setActionInvoked(Notification::ACTION_3); 
			break;
		case SnarlEnums::NotificationClosed:
			notification->setActionInvoked(Notification::CLOSED); 
			break;
		case SnarlEnums::NotificationTimedOut:
			notification->setActionInvoked(Notification::TIMED_OUT); 
			break;
		default:
			qDebug()<<"Unknown snarl action found!!";
			return false;
		}
			_snarl->snore()->notificationActionInvoked(notification);
			return true;
	}
	return false;
}



#include "snarl_backend.moc"
