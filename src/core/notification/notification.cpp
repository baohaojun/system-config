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
#include "snore.h"
#include "notification/icon.h"
#include "plugins/plugincontainer.h"

#include <QDebug>
#include <QTcpSocket>
#include <Qt>
#include <QTextDocumentFragment>
#include <QTextDocument>

namespace Snore{

int Notification::notificationMetaID = qRegisterMetaType<Notification>();

int Notification::notificationCount = 0;

uint Notification::m_idCount = 1;

class Notification::NotificationData : public QObject
{
    Q_OBJECT
public:
    NotificationData ( const QString &application,const QString &alert,const QString &title,const QString &text,const SnoreIcon &icon,int timeout,uint id,NotificationEnums::Prioritys::prioritys priority ):
        m_id ( id == 0 ?m_idCount++:id),
        m_timeout ( timeout ),
        m_source ( NULL),
        m_application ( application ),
        m_alert ( alert ),
        m_title ( title ),
        m_text ( text ),
        m_icon ( icon ),
        m_priority(priority),
        m_closeReason(NotificationEnums::CloseReasons::NONE)
    {
        qDebug()<< "Creating Notification: ActiveNotifications" << ++notificationCount << "id" << m_id;
        m_ref.ref();
    }


    ~NotificationData(){
        qDebug() << "Cloasing Notification:" << m_id << m_ref;
        qDebug() << "Deleting Notification: ActiveNotifications"<<--notificationCount;
    }

    QAtomicInt m_ref;
    uint m_id;
    int m_timeout;
    Notification::Action *m_actionInvoked;
    SnoreFrontend *m_source;
    QString m_application;
    QString m_alert;
    QString m_title;
    QString m_text;
    SnoreIcon m_icon;
    NotificationEnums::Prioritys::prioritys m_priority;
    NotificationEnums::CloseReasons::closeReasons m_closeReason;
    QHash<int,Notification::Action*> m_actions;
    QVariantHash m_hints;


};


int Notification::DefaultTimeout = 10;

QString Notification::toPlainText ( const QString &string )
{
    if( Qt::mightBeRichText(string))
        return QTextDocumentFragment::fromHtml(string).toPlainText();
    return QString(string);
}

Notification::Notification () :
    d(NULL)
{
}

Notification::Notification ( const QString &application, const QString &alert, const QString &title, const QString &text, const SnoreIcon &icon, int timeout, uint id,NotificationEnums::Prioritys::prioritys priority )
{
    d = new  NotificationData(application,alert,title,text,icon,timeout,id,priority);
}

Notification::Notification ( const Notification &other )
{
    if(other.d)
    {
        other.d->m_ref.ref();
        d = other.d;
    }
    else
    {
        d = NULL;
    }
}

Notification::~Notification()
{
    if(d && !d->m_ref.deref())
    {
        d->deleteLater();
    }
}

Notification &Notification::operator=(const Notification& other)
{
    if(d && !d->m_ref.deref())
    {
        d->deleteLater();
    }
    other.d->m_ref.ref();
    d = other.d;
    return *this;
}

const uint &Notification::id() const
{
    return d->m_id;
}

const SnoreIcon &Notification::icon() const
{
    return d->m_icon;
}

const int &Notification::timeout() const
{
    return d->m_timeout;
}

const Notification::Action *Notification::actionInvoked() const
{
    return d->m_actionInvoked;
}

void Notification::setActionInvoked ( Action *action )
{
    d->m_actionInvoked = action;
}

void Notification::setActionInvoked ( const int &id)
{
    d->m_actionInvoked = d->m_actions[id];
}

void Notification::setSource(SnoreFrontend *source) const{
    d->m_source = source;
}

SnoreFrontend *Notification::source() const
{
    return d->m_source;
}

const QString &Notification::application() const
{
    return d->m_application;
}

const QString &Notification::title() const
{
    return d->m_title;
}

const QString &Notification::text() const
{
    return d->m_text;
}

const QString &Notification::alert() const
{
    return d->m_alert;
}

bool Notification::sticky() const
{
    return d->m_timeout == 0;
}

void Notification::setSticky()
{
    d->m_timeout = 0;
}

const NotificationEnums::Prioritys::prioritys &Notification::priority() const
{
    return d->m_priority;
}

void Notification::addAction(Notification::Action *a) 
{
    qDebug()<<"Added notification"<<a->id<<a->name;
    d->m_actions.insert(a->id,a);
}


const QHash<int, Notification::Action *> &Notification::actions() const
{
    return d->m_actions;
}

const NotificationEnums::CloseReasons::closeReasons &Notification::closeReason(){
    return d->m_closeReason;
}

void Notification::setCloseReason(const NotificationEnums::CloseReasons::closeReasons &r){
    d->m_closeReason = r;
}

const QVariant Notification::hint ( const QString &key ) const
{
    return d->m_hints.value ( key );
}

bool Notification::hintExists ( const QString &key )
{
    return d->m_hints.contains ( key );
}

void Notification::insertHint ( const QString &key, const QVariant &val )
{
    d->m_hints.insert ( key,val );
}

bool Notification::isValid() const
{
    return d != NULL;
}

const QObject *Notification::data() const
{
    return d;
}

QDataStream & operator<< ( QDataStream &stream, const Notification &noti )
{
    stream << "Title: " << noti.title() << " Text: " << noti.text() << " ID: " << noti.id() ;
    return stream;
}

QDataStream & operator<< ( QDataStream &stream, const Notification::Action &a)
{
    stream<<a.id<<a.id;
    return stream;
}
}

#include "notification.moc"
