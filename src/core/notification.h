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
#include "snore_exports.h"
#include "application.h"

#include <QVariant>



class SNORE_EXPORT Notification:public QObject
{
    Q_OBJECT
    friend class SnoreServer;
public:
    static int DefaultTimeout;
    static QString toPlainText(const QString &string);
public:    
    Notification(uint id=0);
    Notification(class Notification_Frontend *source,QString title,QString text,QString icon,int timeout=10,uint id=0);
    QString toString() const;
    bool isNotification();
    void setIsNotification(bool b);

    enum actions{
        TIMED_OUT=0,
        ACTION_1=1,
        ACTION_2=2,
        ACTION_3=3,
        CLOSED=4
           };



    const uint &id() const;
    const int &timeout() const;
    const Notification::actions &actionInvoked() const;
    const class Notification_Frontend *source() const;
    const QString &application() const;
    const QString &title() const;
    const QString &text() const;
    const QString &icon() const;
    const QString &alert() const;
    const QVariant hint(const QString &key) const;
    bool hintExists(const QString &key);
    void insertHint(const QString &key,const QVariant &val);


private:
    uint _id;
    int _timeout;
    actions _actionInvoked;
    class Notification_Frontend *_source;
    QString _app;
    QString _title;
    QString _text;
    QString _icon;
    QString _alert;
    QVariantHash _hints;

    bool _notification;



};

QDataStream & operator<< ( QDataStream & stream, const Notification & noti);

#endif // NOTIFICATION_H
