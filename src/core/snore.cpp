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


SnoreCore::SnoreCore(QSystemTrayIcon *trayIcon )
{
    SnoreCorePrivate::registerMetaTypes();
    d_ptr = new SnoreCorePrivate(trayIcon );
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
    foreach( PluginContainer *info, PluginContainer::pluginCache(types).values())
    {
        switch(info->type())
        {
        case SnorePlugin::BACKEND:
            break;
        case SnorePlugin::SECONDARY_BACKEND:
        case SnorePlugin::FRONTEND:
        case SnorePlugin::PLUGIN:
            if(!info->load()->initialize( this ))
            {
                info->unload();
                break;
            }
            break;
        default:
            snoreDebug( SNORE_WARNING ) << "Plugin Cache corrupted\n" << info->file() << info->type();
            continue;
        }
        snoreDebug( SNORE_DEBUG ) << info->name() << "is a" << info->type();
        d->m_plugins[info->type()].append(info->name());
    }
    foreach( SnorePlugin::PluginTypes type, PluginContainer::types())
    {
        qSort(d->m_plugins[type]);
    }
    snoreDebug( SNORE_INFO ) << "Loaded Plugins:" << d->m_plugins;
}

void SnoreCore::broadcastNotification ( Notification notification )
{
    Q_D(SnoreCore);
    snoreDebug( SNORE_DEBUG )<<"Broadcasting"<<notification<<"timeout:"<<notification.timeout();
    if ( d->m_notificationBackend != NULL )
    {
        if(notification.isUpdate() && !d->m_notificationBackend->canUpdateNotification())
        {
            requestCloseNotification(notification.old(),Notification::REPLACED);
        }
        d->m_notificationBackend->addActiveNotification(notification);
    }
    emit d->notify ( notification );
}

void SnoreCore::registerApplication(const Application &application)
{
    Q_D(SnoreCore);
    if(!d->m_applications.contains(application.name()))
    {
        snoreDebug( SNORE_DEBUG ) << "Registering Application:" << application;
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


const QStringList SnoreCore::notificationBackends() const
{
    Q_D(const SnoreCore);
    return d->m_plugins.value(SnorePlugin::BACKEND);
}

const QStringList SnoreCore::notificationFrontends() const
{
    Q_D(const SnoreCore);
    return d->m_plugins.value(SnorePlugin::FRONTEND);
}

const QStringList SnoreCore::secondaryNotificationBackends() const
{
    Q_D(const SnoreCore);
    return d->m_plugins.value(SnorePlugin::SECONDARY_BACKEND);
}

bool SnoreCore::setPrimaryNotificationBackend ( const QString &backend )
{
    Q_D(SnoreCore);
    const QHash<QString,PluginContainer *> backends = PluginContainer::pluginCache(SnorePlugin::BACKEND);
    if(!backends.contains(backend))
    {
        snoreDebug( SNORE_DEBUG ) << "Unknown Backend:" << backend;
        return false;
    }
    snoreDebug( SNORE_DEBUG ) << "Setting Notification Backend to:" << backend;
    SnoreBackend* b = qobject_cast<SnoreBackend*>(backends.value(backend)->load());
    if(!b->isInitialized())
    {
        if(!b->initialize(this))
        {
            snoreDebug( SNORE_DEBUG ) << "Failed to initialize" << b->name();
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
    Q_D(SnoreCore);
#ifdef Q_OS_WIN
    if(QSysInfo::windowsVersion() == QSysInfo::WV_WINDOWS8 && d->setBackendIfAvailible("Windows 8"))
    {
        return true;
    }
    if(d->setBackendIfAvailible("Growl"))
    {
        return true;
    }
    if(d->setBackendIfAvailible("Snarl"))
    {
        return true;
    }
#elif defined(Q_OS_LINUX)
    return d->setBackendIfAvailible("FreedesktopNotification");
#elif defined(Q_OS_MAC)
    return d->setBackendIfAvailible("Growl");
#endif
    if( trayIcon() && d->setBackendIfAvailible("SystemTray"))
    {
        return true;
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

void SnoreCore::requestCloseNotification(Notification n, Notification::CloseReasons r)
{
    Q_D(SnoreCore);
    d->m_notificationBackend->requestCloseNotification(n,r);
}

bool SnoreCore::primaryBackendSupportsRichtext()
{
    Q_D(SnoreCore);
    return d->m_notificationBackend->supportsRichtext();
}

const SnoreCorePrivate *SnoreCore::d()
{
    Q_D(SnoreCore);
    return d;
}
