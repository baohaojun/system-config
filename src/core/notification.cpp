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
#include "snoreserver.h"

#include <QDebug>
#include <QTcpSocket>
#include <Qt>
#include <QTextDocumentFragment>
#include <QTextDocument>


class Notification::NotificationData
{
public:
	NotificationData ( uint id=0 ):
	  _id ( id ),
	_timeout ( 10 ),
	_source ( NULL ),
	_closeReason(Notification::NONE),
	_priority(Notification::NORMAL)
	  {};

	NotificationData ( Notification_Frontend *source,const QString &application,const QString &alert,const QString &title,const QString &text,const QString &icon,int timeout,uint id,Notification::prioritys priority ):
	  	_id ( id ),
	_timeout ( timeout ),
	_source ( source ),
	_application ( application ),
	_alert ( alert ),
	_title ( title ),
	_text ( text ),
	_icon ( icon ),
	_priority(priority),
	_closeReason(Notification::NONE)
		{};


	~NotificationData(){
		//delete _actionInvoked;
	};

	uint _id;
	int _timeout;
	Action *_actionInvoked;
	Notification_Frontend *_source;
	QString _application;
	QString _alert;
	QString _title;
	QString _text;
	QString _icon;
	Notification::prioritys _priority;
	Notification::closeReasons _closeReason;
	QMap<int,Action*> _actions;
	QVariantHash _hints;
};


int Notification::DefaultTimeout=10;

QString Notification::toPlainText ( const QString &string )
{
	if( Qt::mightBeRichText(string))
		return QTextDocumentFragment::fromHtml(string).toPlainText();
	return QString(string);
}

Notification::Notification ( uint id ) 
{
	d = QSharedPointer<NotificationData>(new NotificationData(id));
}

Notification::Notification ( Notification_Frontend *source, const QString &application, const QString &alert, const QString &title, const QString &text, const QString &icon, int timeout, uint id, Notification::prioritys priority ) 
{
	d = QSharedPointer<NotificationData>(new  NotificationData(source,application,alert,title,text,icon,timeout,id,priority));
}

Notification::Notification ( const Notification &other ):
d(other.d)
{
}

Notification::~Notification(){
}

Notification &Notification::operator=(const Notification& other)
{
    d = other.d;
    return *this;
}

QString Notification::toString() const
{
	return QString ( "Title: "+d->_title+"\nText: "+d->_text );
}

const uint &Notification::id() const
{
	return d->_id;
}

void Notification::setId(const uint &id) 
{
	qDebug()<<"setting notification id:"<<id;
	d->_id = id;
}

const QString &Notification::icon() const
{
	return d->_icon;
}

const int &Notification::timeout() const
{
	return d->_timeout;
}

const Action *Notification::actionInvoked() const
{
	return d->_actionInvoked;
}

void Notification::setActionInvoked ( Action *action )
{
	d->_actionInvoked = action;
}

void Notification::setActionInvoked ( const int &id)
{
	d->_actionInvoked = d->_actions[id];
}

Notification_Frontend *Notification::source() const
{
	return d->_source;
}

const QString &Notification::application() const
{
	return d->_application;
}

const QString &Notification::title() const
{
	return d->_title;
}

const QString &Notification::text() const
{
	return d->_text;
}

const QString &Notification::alert() const
{
	return d->_alert;
}

const Notification::prioritys &Notification::priority() const
{
	return d->_priority;
}

void Notification::addAction(Action *a) 
{
	qDebug()<<"Added notification"<<a->id<<a->name;
	d->_actions.insert(a->id,a);
}


const QMap<int,Action*> &Notification::actions() const
{
	return d->_actions;
}

const Notification::closeReasons &Notification::closeReason(){
	return d->_closeReason;
}

void Notification::setCloseReason(const Notification::closeReasons &r){
	d->_closeReason = r;
}

const QVariant Notification::hint ( const QString &key ) const
{
	return d->_hints.value ( key );
}

bool Notification::hintExists ( const QString &key )
{
	return d->_hints.contains ( key );
}

void Notification::insertHint ( const QString &key, const QVariant &val )
{
	d->_hints.insert ( key,val );
}

QDataStream & operator<< ( QDataStream &stream, const Notification &noti )
{
	stream<<noti.toString();
	return stream;
}

QDataStream & operator<< ( QDataStream &stream, const Action &a)
{
	stream<<a.id<<a.id;
	return stream;
}


