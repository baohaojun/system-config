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

#ifndef DBUSBINDING_H
#define DBUSBINDING_H
#include <QtDBus>
#include "core/interface.h"
#include "core/application.h"
#include "core/snoreserver.h"


class DBusPlugin;

class DBusBinding: public QDBusAbstractAdaptor{
    Q_OBJECT
    Q_CLASSINFO("D-Bus Interface", "org.SnoreNotify")
public:
    DBusBinding(DBusPlugin* db,SnoreServer* snore);
    ~DBusBinding();
    static void registerTypes();
private:
    QPointer<SnoreServer> snore;

public slots:
    void setAlertActive(const QString &application,const QString &alert,const bool active);

signals:
    void applicationListChanged(const ApplicationsList &);

private slots:
    void applicationListChangedSlot();

};

class DBusPlugin:public QObject,SnorePlugin{
    Q_OBJECT
    Q_INTERFACES(SnorePlugin)
public:
    DBusPlugin(class SnoreServer *snore=0);

};


Q_DECLARE_METATYPE(ApplicationsList);
QDBusArgument &operator<<(QDBusArgument &a, const ApplicationsList &ap);
const QDBusArgument & operator >>(const QDBusArgument &a, ApplicationsList  &ap) ;

Q_DECLARE_METATYPE(Application);
QDBusArgument &operator<<(QDBusArgument &a, const Application &ap);
const QDBusArgument & operator >>(const QDBusArgument &a, Application  &ap) ;

Q_DECLARE_METATYPE(Alert);
QDBusArgument &operator<<(QDBusArgument &a, const Alert &al);
const QDBusArgument & operator >>(const QDBusArgument &a, Alert &al) ;

Q_DECLARE_METATYPE(AlertList);
QDBusArgument &operator<<(QDBusArgument &a, const AlertList &al);
const QDBusArgument & operator >>(const QDBusArgument &a, AlertList &al) ;






#endif // DBUSBINDING_H
