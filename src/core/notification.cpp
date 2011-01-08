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




int Notification::DefaultTimeout=10;

QString Notification::toPlainText ( const QString &string )
{
    if( Qt::mightBeRichText(string))
        return QTextDocumentFragment::fromHtml(string).toPlainText();
    return QString(string);
}

Notification::Notification ( uint id ) :
        _id ( id ),
        _timeout ( 10 ),
        _source ( NULL ),
        _notification ( true )
{}

Notification::Notification ( Notification_Frontend *source, const QString &application, const QString &alert, const QString &title, const QString &text, const QString &icon, int timeout, uint id ) :
        _id ( id ),
        _timeout ( timeout ),
        _source ( source ),
        _application ( application ),
        _alert ( alert ),
        _title ( title ),
        _text ( text ),
        _icon ( icon ),
        _notification ( true )
{}

QString Notification::toString() const
{
    return QString ( "Title: "+_title+"\nText: "+_text );
}

bool Notification::isNotification()
{
    return _notification;
}

void Notification::setIsNotification ( bool b )
{
    _notification=b;
}
const uint &Notification::id() const
{
    return _id;
}

const QString &Notification::icon() const
{
    return _icon;
}

const int &Notification::timeout() const
{
    return _timeout;
}

const Notification::actions &Notification::actionInvoked() const
{
    return _actionInvoked;
}

void Notification::setActionInvoked ( const Notification::actions &action )
{
    _actionInvoked = action;
}

const Notification_Frontend *Notification::source() const
{
    return _source;
}

const QString &Notification::application() const
{
    return _application;
}

const QString &Notification::title() const
{
    return _title;
}

const QString &Notification::text() const
{
    return _text;
}

const QString &Notification::alert() const
{
    return _alert;
}

const QVariant Notification::hint ( const QString &key ) const
{
    return _hints.value ( key );
}

bool Notification::hintExists ( const QString &key )
{
    return _hints.contains ( key );
}

void Notification::insertHint ( const QString &key, const QVariant &val )
{
    _hints.insert ( key,val );
}

QDataStream & operator<< ( QDataStream &stream, const Notification &noti )
{
    stream<<noti.toString();
    return stream;
}

#include "notification.moc"
