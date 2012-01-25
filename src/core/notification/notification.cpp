/****************************************************************************************
 * Copyright (c) 2010-2012 Patrick von Reth <patrick.vonreth@gmail.com>                 *
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
#include "notification/icon.h"

#include <QDebug>
#include <QTcpSocket>
#include <Qt>
#include <QTextDocumentFragment>
#include <QTextDocument>
namespace Snore{

static int metaid = qRegisterMetaType<Notification>();

static int count = 0;

class Notification::NotificationData : public QObject
{
	Q_OBJECT
public:
    NotificationData ( uint id=0 ):
        _id ( id ),
	_timeout ( 10 ),
	_source ( NULL ),
	_closeReason(NotificationEnums::CloseReasons::NONE),
	_priority(NotificationEnums::Prioritys::NORMAL)
    {
        qDebug()<<"ActiveNotifications"<<++count;
    }

    NotificationData ( const QString &application,const QString &alert,const QString &title,const QString &text,const SnoreIcon &icon,int timeout,uint id,NotificationEnums::Prioritys::prioritys priority ):
        _id ( id ),
	_timeout ( timeout ),
	_source ( NULL),
	_application ( application ),
	_alert ( alert ),
	_title ( title ),
	_text ( text ),
	_icon ( icon ),
	_priority(priority),
	_closeReason(NotificationEnums::CloseReasons::NONE)
    {
        qDebug()<<"ActiveNotifications"<<++count;
    }


    ~NotificationData(){
        //delete _actionInvoked;
        qDebug()<<"ActiveNotifications"<<--count;
    }

    uint _id;
    int _timeout;
    Notification::Action *_actionInvoked;
    Notification_Frontend *_source;
    QString _application;
    QString _alert;
    QString _title;
    QString _text;
    SnoreIcon _icon;
    NotificationEnums::Prioritys::prioritys _priority;
    NotificationEnums::CloseReasons::closeReasons _closeReason;
    QMap<int,Notification::Action*> _actions;
    QVariantHash _hints;
};


int Notification::DefaultTimeout = 10;

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

Notification::Notification ( const QString &application, const QString &alert, const QString &title, const QString &text, const SnoreIcon &icon, int timeout, uint id, NotificationEnums::Prioritys::prioritys priority ) 
{
    d = QSharedPointer<NotificationData>(new  NotificationData(application,alert,title,text,icon,timeout,id,priority));
}

Notification::Notification ( const Notification &other ):
    d(other.d)
{
}

Notification::~Notification()
{
    d.clear();
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

const SnoreIcon &Notification::icon() const
{
    return d->_icon;
}

const int &Notification::timeout() const
{
    return d->_timeout;
}

const Notification::Action *Notification::actionInvoked() const
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

void Notification::setSource(Notification_Frontend *source) const{
    d->_source = source;
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

bool Notification::sticky() const
{
    return d->_timeout == 0;
}

void Notification::setSticky()
{
    d->_timeout = 0;
}

const NotificationEnums::Prioritys::prioritys &Notification::priority() const
{
    return d->_priority;
}

void Notification::addAction(Notification::Action *a) 
{
    qDebug()<<"Added notification"<<a->id<<a->name;
    d->_actions.insert(a->id,a);
}


const QMap<int,Notification::Action*> &Notification::actions() const
{
    return d->_actions;
}

const NotificationEnums::CloseReasons::closeReasons &Notification::closeReason(){
    return d->_closeReason;
}

void Notification::setCloseReason(const NotificationEnums::CloseReasons::closeReasons &r){
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

QDataStream & operator<< ( QDataStream &stream, const Notification::Action &a)
{
    stream<<a.id<<a.id;
    return stream;
}
}


#include <notification.moc>
