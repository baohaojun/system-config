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

#include "snoreserver.h"
#include "notification/notification.h"
#include "version.h"

#include <iostream>

#include <QPluginLoader>
#include <QDebug>
#include <QDir>
#include <QSystemTrayIcon>
#include <QApplication>
#include <QSettings>

namespace Snore{

QHash<QString,SnorePluginInfo*> SnoreServer::m_pluginCache = QHash<QString,SnorePluginInfo*>() ;

QString const SnoreServer::version(){
    return QString().append(Version::major()).append(".").append(Version::minor()).append(Version::suffix());
}

QString const SnoreServer::snoreTMP(){
    static const QString tmp = QDir::temp().path() +"/SnoreNotify/";
    return tmp;
}


SnoreServer::SnoreServer ( QSystemTrayIcon *trayIcon ) :
    m_notificationBackend ( NULL ),
    m_trayIcon ( trayIcon )
{    
    QDir home ( snoreTMP() );
    if ( !home.exists() ){
        home.cdUp();
        home.mkdir("SnoreNotify");
    }

}

QHash<QString, SnorePluginInfo *> SnoreServer::pluginCache(const QString &pluginPath){
    if(!m_pluginCache.isEmpty())
        return m_pluginCache;
    QSettings cache(SnoreServer::pluginDir(pluginPath).absoluteFilePath("plugin.cache"),QSettings::IniFormat);
    int size = cache.beginReadArray("plugins");
    if(size == 0)
        return updatePluginCache(pluginPath);


    for(int i=0;i<size;++i) {
        cache.setArrayIndex(i);
        SnorePluginInfo *info  = new SnorePluginInfo();
        info->pluginFile = cache.value("fileName").toString();
        info->pluginName = cache.value("name").toString();
        info->pluginType = (SnorePluginInfo::type)cache.value("type").toInt();
        m_pluginCache.insert(info->pluginName,info);
    }
    cache.endArray();

    return m_pluginCache;
}

QHash<QString, SnorePluginInfo *> SnoreServer::updatePluginCache(const QString &pluginPath){
    QSettings cache(SnoreServer::pluginDir(pluginPath).absoluteFilePath("plugin.cache"),QSettings::IniFormat);
    qDebug()<<"Updating plugin cache"<<cache.fileName();

    m_pluginCache.clear();

    foreach(const QString &type,SnorePluginInfo::types()){
        QDir plPath(SnoreServer::pluginDir(pluginPath).absoluteFilePath(type));
        qDebug()<<"Searching for plugins in"<<plPath.path();
        foreach (QString fileName, plPath.entryList(QDir::Files)) {
            QString filepath(plPath.absoluteFilePath(fileName));
            qDebug()<<"adding"<<filepath;
            QPluginLoader loader(filepath);
            QObject *plugin = loader.instance();
            if (plugin == NULL) {
                qDebug()<<"Failed loading plugin: "<<filepath<<loader.errorString();
                continue;
            }
            SnorePlugin *sp = dynamic_cast<SnorePlugin*>(plugin);
            if(sp == NULL){
                qDebug()<<"Error:"<<fileName.toLatin1().data()<<" is not a Snore plugin" ;
                plugin->deleteLater();
                continue;
            }
            SnorePluginInfo *info = new SnorePluginInfo();
            info->pluginFile = SnoreServer::pluginDir(pluginPath).relativeFilePath(filepath);
            info->pluginName = sp->name();
            info->pluginType = SnorePluginInfo::typeFromString(type);
            m_pluginCache.insert(info->pluginName,info);
            sp->deleteLater();
            qDebug()<<"added "<<info->pluginFile<<"to cache";
        }
    }

    qDebug()<<m_pluginCache.keys();
    QList<SnorePluginInfo*> plugins = m_pluginCache.values();
    cache.beginWriteArray("plugins");
    for(int i=0;i< plugins.size();++i) {
        cache.setArrayIndex(i);
        cache.setValue("fileName",plugins[i]->pluginFile);
        cache.setValue("name", plugins[i]->pluginName);
        cache.setValue("type",plugins[i]->pluginType);
    }
    cache.endArray();
    return m_pluginCache;
}

const QDir &SnoreServer::pluginDir(const QString &pluginPath){
    static QDir *plDir = NULL;
    if(plDir == NULL){
        if(!pluginPath.isEmpty())
            plDir = new QDir(pluginPath);
        if(pluginPath.isEmpty() || plDir->exists()){
            plDir = new QDir(qApp->applicationDirPath()+"/snoreplugins");
            if(!plDir->exists())
                plDir = new QDir(LIBSNORE_PLUGIN_PATH);
        }
    }
    return *plDir;
}

void SnoreServer::publicatePlugin ( const SnorePluginInfo *info )
{
    QPluginLoader loader ( SnoreServer::pluginDir(QString()).absoluteFilePath(info->pluginFile ));
    qDebug()<<"Trying to load"<<info->pluginFile;
    if ( !loader.load())
    {
        qDebug() <<"Failed loading plugin: "<<loader.errorString();
        return;
    }

    SnorePlugin *plugin = qobject_cast<SnorePlugin*> ( loader.instance());
    m_plugins.insert ( info->pluginName ,plugin );

    switch(info->pluginType){
    case SnorePluginInfo::BACKEND:{
        Notification_Backend * nb = qobject_cast<Notification_Backend *> ( plugin );
        qDebug() <<info->pluginName<<"is a Notification_Backend";
        if ( nb->isPrimaryNotificationBackend() )
        {
            m_primaryNotificationBackends.append( info->pluginName);
            if ( m_notificationBackend == NULL )
            {
                m_notificationBackend = nb;
            }
        }else{
            if(!nb->init( this )){
                nb->deleteLater();
                return;
            }

            m_notyfier.insert ( info->pluginName,nb );
        }
        break;
    }
    case SnorePluginInfo::FRONTEND:{
        Notification_Frontend * nf = qobject_cast<Notification_Frontend*> (plugin);
        qDebug() <<info->pluginName<<"is a Notification_Frontend";
        if(nf->init( this ))
            m_frontends.insert(nf->name(),nf);
        else
            nf->deleteLater();
        break;
    }
    case SnorePluginInfo::PLUGIN:
        qDebug() <<info->pluginName<<"is a SnorePlugin";
        plugin->init(this);
    break;
    default:
        std::cerr<<"Plugin Cache corrupted"<<std::endl ;
        break;
    }
}

uint SnoreServer::broadcastNotification ( Notification notification )
{
    qDebug()<<"Broadcasting"<<notification.title()<<notification.timeout();
    emit notify ( notification );
    if ( m_notificationBackend != NULL )
    {
        if(!m_notificationBackend->isInitialized()){
            qDebug()<<"Notification backend "<<m_notificationBackend<<" isnt initialized will snore will exit now";
            qApp->quit();
        }
        notification.setId(m_notificationBackend->notify( notification ));
        return  notification.id();
    }
    return -1;
}

void SnoreServer::closeNotification ( Notification notification,const NotificationEnums::CloseReasons::closeReasons &reason )
{
    notification.setCloseReason(reason);
    emit closeNotify ( notification );
}

void SnoreServer::notificationActionInvoked ( Notification notification )
{
    emit actionInvoked(notification);
    Notification_Frontend *nf= notification.source();
    if ( nf != NULL )
    {
        nf->actionInvoked ( notification );
    }
}

void SnoreServer::addApplication ( Application *application )
{
    m_applications.insert ( application->name(),application );
}

void SnoreServer::applicationIsInitialized ( Application *application )
{
    application->setInitialized ( true );
    emit applicationInitialized ( application );
}

void SnoreServer::removeApplication ( const QString& appName )
{
    qDebug()<<"Remove Application"<<appName;
    emit applicationRemoved ( m_applications.value ( appName ) );
    m_applications.take ( appName )->deleteLater();
}

const ApplicationsList &SnoreServer::aplications() const
{
    return m_applications;
}


const QStringList &SnoreServer::primaryNotificationBackends() const
{
    return m_primaryNotificationBackends;
}

void SnoreServer::setPrimaryNotificationBackend ( const QString &backend )
{
    if(!m_primaryNotificationBackends.contains(backend))
        return;
    qDebug()<<"Setting Notification Backend to:"<<backend;
    m_notificationBackend = qobject_cast<Notification_Backend*>(m_plugins[backend]);
    if(!m_notificationBackend->isInitialized())
        m_notificationBackend->init(this);
}

const QString &SnoreServer::primaryNotificationBackend(){
    return m_notificationBackend->name();
}

QSystemTrayIcon *SnoreServer::trayIcon(){
    return m_trayIcon;
}

}

#include "snoreserver.moc"
