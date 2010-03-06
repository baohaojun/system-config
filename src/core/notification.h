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
#include <QHash>
#include <QFile>
#include "application.h"
#include <Qt>
#include <QTextEdit>




class SNORE_EXPORT Notification:public QObject
{
    Q_OBJECT
    friend class SnoreServer;
public:
    static int DefaultTimeout;
    static inline QString toPlainText(const QString &string){
        if(!Qt::mightBeRichText ( string))return string;
        QTextEdit te;
        te.setHtml(string);
        return te.toPlainText();
    };
public:    
    Notification(uint id=0);
    Notification(QString source,QString title,QString text,QString icon,int timeout=10,uint id=0);
    bool isNotification();
    void setIsNotification(bool b);
    QString toSnalrString()const;

    enum actions{
        TIMED_OUT=0,
        ACTION_1=1,
        ACTION_2=2,
        ACTION_3=3,
        CLOSED=4

           };

    actions actionInvoked;
    QString source;
    QString app;
    QString title;
    QString text;
    QString alert;
    int timeout;      
    void setIcon(const QString &icon){this->icon=icon; }
    QString getIcon();
    QVariantHash hints;
    uint getID();



private:
    uint id;
    QString icon;
    bool notification;



};

QDataStream & operator<< ( QDataStream & stream, const Notification & noti);

#endif // NOTIFICATION_H
