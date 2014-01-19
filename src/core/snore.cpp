/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2014  Patrick von Reth <vonreth@kde.org>


    SnoreNotify is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SnoreNotify is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with SnoreNotify.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "snore.h"
#include "snore_p.h"
#include "notification/notification.h"
#include "plugins/plugincontainer.h"
#include "plugins/plugins.h"
#include "plugins/snorebackend.h"
#include "plugins/snorefrontend.h"

#include "version.h"

#include <iostream>

#include <QDebug>
#include <QDir>
#include <QSystemTrayIcon>
#include <QApplication>
#include <QSettings>

using namespace Snore;


SnoreCore::SnoreCore(QSystemTrayIcon *trayIcon ):
    d_ptr(new SnoreCorePrivate(trayIcon ))
{
    Q_D(SnoreCore);
    d->q_ptr = this;
}

SnoreCore::~SnoreCore()
{
    Q_D(SnoreCore);
    d->deleteLater();
}

void SnoreCore::loadPlugins( SnorePlugin::PluginTypes types )
{
    Q_D(SnoreCore);
    qDebug() << "PluginInfo" << PluginContainer::pluginCache().keys();
    foreach ( PluginContainer *info, PluginContainer::pluginCache().values())
    {
        if(types == SnorePlugin::ALL || types.testFlag(info->type()))
        {
            switch(info->type())
            {
            case SnorePlugin::BACKEND:
            {
                qDebug() << info->name() << "is a Notification_Backend";
                d->m_notificationBackends.append( info->name());
                break;
            }
            case SnorePlugin::SECONDARY_BACKEND:
            {
                if(!info->load()->initialize( this )){
                    info->unload();
                    break;
                }
                d->m_secondaryNotificationBackends.append(info->name());
                break;
            }
            case SnorePlugin::FRONTEND:
            {
                qDebug() << info->name() << "is a Notification_Frontend";
                if(!info->load()->initialize( this )){
                    info->unload();
                    break;
                }
                d->m_Frontends.append(info->name());
                break;
            }
            case SnorePlugin::PLUGIN:
            {
                qDebug() <<info->name()<<"is a SnorePlugin";
                if(!info->load()->initialize(this)){
                    info->unload();
                    break;
                }
                d->m_plugins.append(info->name());
                break;
            }
            default:
            {
                std::cerr<<"Plugin Cache corrupted"<<std::endl;
                std::cerr<<info->file().toLocal8Bit().constData()<<QString::number((int)info->type()).toLatin1().constData()<<std::endl;
            }
            }
        }else{
            qDebug()<<"dont load "<<info->file()<<info->type();
        }
    }
}

void SnoreCore::broadcastNotification ( Notification notification )
{
    Q_D(SnoreCore);
    qDebug()<<"Broadcasting"<<notification<<"timeout:"<<notification.timeout();
    emit d->notify ( notification );
    if ( d->m_notificationBackend != NULL )
    {
        d->m_notificationBackend->addActiveNotification(notification);
    }
}

void SnoreCore::registerApplication(const Application &application)
{
    Q_D(SnoreCore);
    if(!d->m_applications.contains(application.name()))
    {
        qDebug() << Q_FUNC_INFO << "Registering Application:" << application;
        d->m_applications.insert ( application.name(),application );
        emit d->applicationRegistered ( application );
    }
}

void SnoreCore::deregisterApplication(const Application &application)
{
    Q_D(SnoreCore);
    emit d->applicationDeregistered (application );
    d->m_applications.take ( application.name() );
}

const QHash<QString, Application> &SnoreCore::aplications() const
{
    Q_D(const SnoreCore);
    return d->m_applications;
}


const QStringList &SnoreCore::notificationBackends() const
{
    Q_D(const SnoreCore);
    return d->m_notificationBackends;
}

const QStringList &SnoreCore::notificationFrontends() const
{
    Q_D(const SnoreCore);
    return d->m_Frontends;
}

const QStringList &SnoreCore::secondaryNotificationBackends() const
{
    Q_D(const SnoreCore);
    return d->m_secondaryNotificationBackends;
}

bool SnoreCore::setPrimaryNotificationBackend ( const QString &backend )
{
    Q_D(SnoreCore);
    if(!PluginContainer::pluginCache().contains(backend))
    {
        qDebug() << "Unknown Backend:" << backend;
        return false;
    }
    qDebug() << "Setting Notification Backend to:" << backend;
    SnoreBackend* b = qobject_cast<SnoreBackend*>(PluginContainer::pluginCache()[backend]->load());
    if(!b->isInitialized())
    {
        if(!b->initialize(this))
        {
            qDebug() << "Failed to initialize" << b->name();
            return false;
        }
    }
    if(d->m_notificationBackend)
    {
        d->m_notificationBackend->deinitialize();
    }

    d->m_notificationBackend = b;
    return true;
}

bool SnoreCore::setPrimaryNotificationBackend()
{
    QStringList backends = notificationBackends();
#ifdef Q_OS_WIN
    if(QSysInfo::windowsVersion() == QSysInfo::WV_WINDOWS8 && backends.contains("Windows 8"))
    {
        if(setPrimaryNotificationBackend("Windows 8"))
            return true;
    }
    if( backends.contains("Growl"))
    {
        if(setPrimaryNotificationBackend("Growl"))
            return true;
    }
    if( backends.contains("Snarl"))
    {
        if(setPrimaryNotificationBackend("Snarl"))
            return true;
    }
#elif defined(Q_OS_LINUX)
    if( backends.contains("FreedesktopNotification_Backend"))
    {
        return setPrimaryNotificationBackend("FreedesktopNotification");
    }
#elif defined(Q_OS_MAC)
    if( backends.contains("Growl"))
    {
        return setPrimaryNotificationBackend("Growl");
    }
#endif
    if( trayIcon() && backends.contains("SystemTray"))
    {
        return setPrimaryNotificationBackend("SystemTray");
    }
    return false;
}

const QString SnoreCore::primaryNotificationBackend() const
{
    Q_D(const SnoreCore);
    if(d->m_notificationBackend.isNull())
    {
        return QString::null;
    }
    return d->m_notificationBackend->name();
}

QSystemTrayIcon *SnoreCore::trayIcon(){
    Q_D(SnoreCore);
    return d->m_trayIcon;
}

Notification SnoreCore::getActiveNotificationByID(uint id)
{
    Q_D(SnoreCore);
    if(!d->m_notificationBackend->isInitialized())
    {
        qFatal("Notification backend %s isn't initialized will snore will exit now",d->m_notificationBackend->name().toLatin1().constData());
    }
    return d->m_notificationBackend->getActiveNotificationByID(id);
}

void SnoreCore::requestCloseNotification(Notification n, NotificationEnums::CloseReasons::closeReasons r)
{
    Q_D(SnoreCore);
    d->m_notificationBackend->requestCloseNotification(n,r);
}

bool SnoreCore::primaryBackendSupportsRichtext()
{
    Q_D(SnoreCore);
    return d->m_notificationBackend->supportsRichtext();
}

Hint &SnoreCore::hints()
{
    Q_D(SnoreCore);
    return d->m_hints;
}

const SnoreCorePrivate *SnoreCore::d()
{
    Q_D(SnoreCore);
    return d;
}
