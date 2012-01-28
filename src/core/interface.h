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

#ifndef INTERFACE_H
#define INTERFACE_H
#include "snore_exports.h"
#include "notification/notification.h"

#include <QPointer>
#include <QFlag>

namespace Snore{
class Application;
class SnoreServer;
class SnorePlugin;

class SNORE_EXPORT SnorePluginInfo{
public:
    enum PluginType{
        ALL = 0x0,//dor loading plugins
        BACKEND = 0x1,
        SECONDARY_BACKEND = 0x2,
        FRONTEND = 0x4,
        PLUGIN = 0x8
    };
    Q_DECLARE_FLAGS(PluginTypes, PluginType)
    SnorePluginInfo(QString fileName,QString pluginName,PluginType type);
    SnorePlugin *load();
    const QString &file();
    const QString &name();
    const SnorePluginInfo::PluginType type();


    static Snore::SnorePluginInfo::PluginType typeFromString(const QString &t);
    static const QStringList &types();

private:
    QPointer<SnorePlugin> m_instance;
    QString m_pluginFile;
    QString m_pluginName;
    Snore::SnorePluginInfo::PluginType m_pluginType;
};

class SNORE_EXPORT SnorePlugin:public QObject
{
    Q_OBJECT
public:
    SnorePlugin ( QString name);
    virtual ~SnorePlugin();
    virtual bool init( SnoreServer* snore );
    bool isInitialized();
    SnoreServer* snore();
    const QString &name() const;

protected:
    QHash<uint,Notification> activeNotifications;
    void startTimeout(uint id,int timeout);
private slots:
    void notificationTimedOut();

private:
    SnorePlugin() {}
    QString m_name;
    bool m_initialized;
    QPointer<SnoreServer> m_snore;
    QHash<uint,QTimer*> m_timeouts;
    QList<uint> m_timeout_order;


};

}
Q_DECLARE_OPERATORS_FOR_FLAGS(Snore::SnorePluginInfo::PluginTypes)
Q_DECLARE_INTERFACE ( Snore::SnorePlugin,
                      "org.Snore.SnorePlugin/1.0" )

namespace Snore{

class SNORE_EXPORT Notification_Backend:public SnorePlugin
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnorePlugin)
public:
    Notification_Backend ( QString name );
    virtual ~Notification_Backend();
    virtual bool init(SnoreServer *snore);



public slots:
    virtual void registerApplication ( Snore::Application *application ) =0;
    virtual void unregisterApplication ( Snore::Application *application ) =0;
    virtual uint notify ( Snore::Notification notification ) =0;
    virtual void closeNotification ( Snore::Notification notification ) =0;



};

}
Q_DECLARE_INTERFACE ( Snore::Notification_Backend,
                      "org.Snore.NotificationBackend/1.0" )

namespace Snore{

class SNORE_EXPORT Secondary_Notification_Backend:public Notification_Backend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnorePlugin Snore::Notification_Backend)
public:
    Secondary_Notification_Backend(const  QString &name);
    virtual ~Secondary_Notification_Backend();
    virtual bool init(SnoreServer *snore);

};

}

Q_DECLARE_INTERFACE ( Snore::Secondary_Notification_Backend,
                      "org.Snore.SecondaryNotificationBackend/1.0" )

namespace Snore{

class SNORE_EXPORT Notification_Frontend:public SnorePlugin
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnorePlugin)
public:
    Notification_Frontend ( const QString &name);
    virtual ~Notification_Frontend();
    virtual bool init(SnoreServer *snore);

public slots:
    virtual void actionInvoked( Snore::Notification notification )=0;
    virtual void notificationClosed( Snore::Notification notification )=0;
};

}

Q_DECLARE_INTERFACE ( Snore::Notification_Frontend,
                      "org.Snore.NotificationFrontend/1.0" )


#endif//INTERFACE_H
