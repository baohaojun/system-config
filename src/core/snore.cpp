/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013  Patrick von Reth <vonreth@kde.org>


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

void SnoreCore::slotNotificationClosed(Notification n)
{
    emit notificationClosed(n);
}

QString const SnoreCore::snoreTMP(){
    static const QString tmp = QString("%1/SnoreNotify/").arg(QDir::temp().path());
    return tmp;
}


SnoreCore::SnoreCore ( QSystemTrayIcon *trayIcon ) :
    m_trayIcon ( trayIcon )
{
    QDir home ( snoreTMP() );
    if ( !home.exists() ){
        home.cdUp();
        home.mkdir("SnoreNotify");
    }

}

const QDir &SnoreCore::pluginDir(){
    static QDir path(QString("%1/snoreplugins").arg(qApp->applicationDirPath()));
    if(!path.exists())
    {
        path = QDir(LIBSNORE_PLUGIN_PATH);
    }
    qDebug() << "PluginDir" << path.absolutePath();
    return path;
}


void SnoreCore::loadPlugins ( PluginContainer::PluginTypes types )
{
    qDebug() << "PluginInfo" << PluginContainer::pluginCache().keys();
    foreach ( PluginContainer *info, PluginContainer::pluginCache().values())
    {
        if(types == PluginContainer::ALL || types.testFlag(info->type()))
        {
            switch(info->type())
            {
            case PluginContainer::BACKEND:
            {
                qDebug() <<info->name()<<"is a Notification_Backend";
                m_notificationBackends.append( info->name());
                break;
            }
            case PluginContainer::SECONDARY_BACKEND:
            {
                if(!info->load()->init( this )){
                    info->unload();
                    break;
                }
                m_secondaryNotificationBackends.append(info->name());
                break;
            }
            case PluginContainer::FRONTEND:
            {
                qDebug() <<info->name()<<"is a Notification_Frontend";
                if(!info->load()->init( this )){
                    info->unload();
                    break;
                }
                m_Frontends.append(info->name());
                break;
            }
            case PluginContainer::PLUGIN:
            {
                qDebug() <<info->name()<<"is a SnorePlugin";
                if(!info->load()->init(this)){
                    info->unload();
                    break;
                }
                m_plugins.append(info->name());
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
    qDebug()<<"Broadcasting"<<notification<<"timeout:"<<notification.timeout();
    emit notify ( notification );
    if ( m_notificationBackend != NULL )
    {
        if(!m_notificationBackend->isInitialized()){
            qDebug()<<"Notification backend "<<m_notificationBackend<<" isnt initialized will snore will exit now";
            qApp->quit();
        }
        m_notificationBackend->slotNotify( notification );
        m_notificationBackend->addActiveNotification(notification);
    }
}


void SnoreCore::notificationActionInvoked ( Notification notification )
{
    emit actionInvoked(notification);
    SnoreFrontend *nf = notification.source();
    if ( nf != NULL )
    {
        nf->actionInvoked ( notification );
    }
}

void SnoreCore::addApplication ( Application *application )
{
    m_applications.insert ( application->name(),application );
}

void SnoreCore::applicationIsInitialized ( Application *application )
{
    application->setInitialized ( true );
    emit applicationInitialized ( application );
}

void SnoreCore::removeApplication ( const QString& appName )
{
    qDebug()<<"Remove Application"<<appName;
    emit applicationRemoved ( m_applications.value ( appName ) );
    m_applications.take ( appName )->deleteLater();
}

const ApplicationsList &SnoreCore::aplications() const
{
    return m_applications;
}


const QStringList &SnoreCore::notificationBackends() const
{
    return m_notificationBackends;
}

const QStringList &SnoreCore::notificationFrontends() const
{
    return m_Frontends;
}

const QStringList &SnoreCore::secondaryNotificationBackends() const
{
    return m_secondaryNotificationBackends;
}

bool SnoreCore::setPrimaryNotificationBackend ( const QString &backend )
{
    if(!PluginContainer::pluginCache().contains(backend)){
        qDebug()<<"Unknown Backend:"<<backend;
        return false;
    }
    qDebug()<<"Setting Notification Backend to:"<<backend;
    SnoreBackend* b = qobject_cast<SnoreBackend*>(PluginContainer::pluginCache()[backend]->load());
    if(!b->isInitialized()){
        if(!b->init(this)){
            qDebug()<<"Failed to initialize"<<b->name();
            return false;
        }
        connect(b,SIGNAL(closeNotification(Snore::Notification)),this,SLOT(slotNotificationClosed(Snore::Notification)));
    }
    m_notificationBackend = b;
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
        return setPrimaryNotificationBackend("FreedesktopNotification_Backend");
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

const QString SnoreCore::primaryNotificationBackend()
{
    if(m_notificationBackend == NULL)
    {
        return QString();
    }
    return m_notificationBackend->name();
}

QSystemTrayIcon *SnoreCore::trayIcon(){
    return m_trayIcon;
}

Notification SnoreCore::getActiveNotificationByID(uint id)
{
    if(!m_notificationBackend->isInitialized()){
        qDebug()<<"Notification backend "<<m_notificationBackend<<" isn't initialized will snore will exit now";
        qApp->quit();
    }
    return m_notificationBackend->getActiveNotificationByID(id);
}

void SnoreCore::requestCloseNotification(Notification n, NotificationEnums::CloseReasons::closeReasons r)
{
    m_notificationBackend->requestCloseNotification(n,r);
}

bool SnoreCore::primaryBackendSupportsRichtext()
{
    return m_notificationBackend->supportsRichtext();
}

Hint &SnoreCore::hints()
{
    return m_hints;
}
