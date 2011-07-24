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
#include "gntp.h"

#include "core/snoreserver.h"


#include <QtCore>

#include <boost/asio.hpp>

Q_EXPORT_PLUGIN2(growl_backend,Growl_Backend)

	Growl_Backend::Growl_Backend(SnoreServer *snore):
Notification_Backend("Growl",snore),
	id(0)
{
}
Growl_Backend::~Growl_Backend(){
	foreach(Application *a,this->snore()->aplications().values()){
		unregisterApplication(a);
	}
	qDebug()<<"Growl_Backend bye bye";

}

void Growl_Backend::registerApplication(Application *application){
	gntp *growl =  new gntp(application->name().toUtf8().constData(),application->icon().localUrl().toUtf8().constData());

	std::vector<std::string> alerts;
	foreach(Alert *a,application->alerts()){
		alerts.push_back(a->name().toUtf8().constData());
	}

	try{
		growl->regist(alerts);
	}catch(const std::exception& e){
		qDebug()<<"Growl:"<<e.what();
	}
	_applications.insert(application->name(),growl);
}

void Growl_Backend::unregisterApplication(Application *application){
	gntp *growl = _applications.take(application->name());
	if(growl == NULL)
		return;
	delete growl;
}

int Growl_Backend::notify(Notification notification){
	gntp *growl = _applications.value(notification.application());
	if(growl == NULL)
		return -1;

	//qDebug()<<"Notify Growl:"<<notification.application()<<Notification.toPlainText(notification.title());
	try{
		growl->notify(notification.alert().toUtf8().constData(),
			Notification::toPlainText(notification.title()).toUtf8().constData(),
			Notification::toPlainText(notification.text()).toUtf8().constData(),
			notification.icon().localUrl().isEmpty()?NULL:notification.icon().localUrl().toUtf8().constData());
	}catch(const std::exception& e){
	}
	return ++id;
}

void Growl_Backend::closeNotification(Notification notification){
	Q_UNUSED(notification);
}

#include "growl_backend.moc"
