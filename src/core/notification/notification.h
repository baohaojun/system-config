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

#ifndef NOTIFICATION_H
#define NOTIFICATION_H
#include "../snore_exports.h"
#include "../application.h"
#include "icon.h"

#include <QVariant>
#include <QSharedPointer>

namespace NotificationEnums{

namespace Prioritys{
	enum prioritys{
		LOW=-1,
		NORMAL,
		HIGH
	};
}
namespace CloseReasons{
	enum closeReason
    {
		NONE,
		TIMED_OUT,
		DISMISSED,
		CLOSED
    };
	Q_DECLARE_FLAGS(closeReasons, closeReason)
	Q_DECLARE_OPERATORS_FOR_FLAGS(closeReasons)
}
}

class SNORE_EXPORT Notification
{
public:
    static int DefaultTimeout;
    static QString toPlainText ( const QString &string );

	class Action
	{
	public:
		Action(int id,QString name):id(id),name(name){}
		int id;
		QString name;
	};

public:
    Notification ( uint id=0 );
	Notification ( class Notification_Frontend *source,const QString &application,const QString &alert,const QString &title,const QString &text,const NotificationIcon &icon,int timeout=10,uint id=0, NotificationEnums::Prioritys::prioritys priority = NotificationEnums::Prioritys::NORMAL );
	Notification ( const Notification &other );
	~Notification();
	Notification &operator=(const Notification& other);

    QString toString() const;

    const uint &id() const;
	void setId(const uint &id);
    const int &timeout() const;
	//void setActionInvoked ( const Notification::defaultActions &action );
	void setActionInvoked ( Action *action );
	void setActionInvoked ( const int &actionID);
	//const Notification::defaultActions &actionInvoked() const;
	const Action* actionInvoked() const;
	class Notification_Frontend *source() const;
    const QString &application() const;
    const QString &title() const;
    const QString &text() const;
	const NotificationIcon &icon() const;
    const QString &alert() const;
	const NotificationEnums::Prioritys::prioritys &priority() const;
	const QMap<int,Action*> &actions() const;
	void addAction(Action *a);
	const NotificationEnums::CloseReasons::closeReasons &closeReason();
	void setCloseReason(const NotificationEnums::CloseReasons::closeReasons &r);
	const QVariant hint ( const QString &key ) const;
    bool hintExists ( const QString &key );
    void insertHint ( const QString &key,const QVariant &val );


private:
	class NotificationData;
	QSharedPointer<NotificationData> d;
};


QDataStream & operator<< ( QDataStream & stream, const Notification & noti );
QDataStream & operator<< ( QDataStream & stream, const Notification::Action & action);

#endif // NOTIFICATION_H
