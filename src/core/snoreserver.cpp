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

QHash<QString,SnorePluginInfo*> SnoreServer::s_pluginCache = QHash<QString,SnorePluginInfo*>() ;

QString SnoreServer::s_pluginPath = QString(qApp->applicationDirPath()+"/snoreplugins");

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

QHash<QString, SnorePluginInfo *> SnoreServer::pluginCache(){
    if(!s_pluginCache.isEmpty())
        return s_pluginCache;
    QSettings cache(SnoreServer::pluginDir().absoluteFilePath("plugin.cache"),QSettings::IniFormat);
    int size = cache.beginReadArray("plugins");
    if(size == 0){
        updatePluginCache();
    }else{
        for(int i=0;i<size;++i) {
            cache.setArrayIndex(i);
            SnorePluginInfo::PluginType type = (SnorePluginInfo::PluginType)cache.value("type").toInt();
            SnorePluginInfo *info = new SnorePluginInfo(cache.value("fileName").toString(),cache.value("name").toString(),type);
            s_pluginCache.insert(info->name(),info);
        }
        cache.endArray();
    }

    return s_pluginCache;
}

void SnoreServer::updatePluginCache(const QString &pluginPath){
    if(!pluginPath.isEmpty())
        s_pluginPath = pluginPath;
    QSettings cache(SnoreServer::pluginDir().absoluteFilePath("plugin.cache"),QSettings::IniFormat);
    qDebug()<<"Updating plugin cache"<<cache.fileName();

    s_pluginCache.clear();

    foreach(const QString &type,SnorePluginInfo::types()){
        QDir plPath(SnoreServer::pluginDir().absoluteFilePath(type));
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
            SnorePluginInfo *info = new SnorePluginInfo( SnoreServer::pluginDir().relativeFilePath(filepath),sp->name(),SnorePluginInfo::typeFromString(type));
            s_pluginCache.insert(info->name(),info);
            sp->deleteLater();
            qDebug()<<"added "<<info->name()<<"to cache";
        }
    }

    qDebug()<<s_pluginCache.keys();
    QList<SnorePluginInfo*> plugins = s_pluginCache.values();
    cache.beginWriteArray("plugins");
    for(int i=0;i< plugins.size();++i) {
        cache.setArrayIndex(i);
        cache.setValue("fileName",plugins[i]->file());
        cache.setValue("name", plugins[i]->name());
        cache.setValue("type",(int)plugins[i]->type());
    }
    cache.endArray();
}

const QDir &SnoreServer::pluginDir()
{
    //TODO:fix logic
    static QDir *plDir = NULL;
    if(plDir == NULL){
        plDir = new QDir(s_pluginPath);
        if(!plDir->exists())
            plDir = new QDir(LIBSNORE_PLUGIN_PATH);
    }
    return *plDir;
}

void SnoreServer::publicatePlugin ( SnorePluginInfo::PluginTypes types )
{
    foreach ( SnorePluginInfo *info, SnoreServer::pluginCache().values())
    {
        if(types == SnorePluginInfo::ALL or  types.testFlag(info->type())){
            switch(info->type()){
            case SnorePluginInfo::BACKEND:{
                qDebug() <<info->name()<<"is a Notification_Backend";
                m_notificationBackends.append( info->name());
                break;
            }
            case SnorePluginInfo::SECONDARY_BACKEND:{
                Secondary_Notification_Backend *nb = qobject_cast<Secondary_Notification_Backend *> ( info->load() );
                if(!nb->init( this )){
                    nb->deleteLater();
                    break;
                }
                m_secondaryNotificationBackends.append(info->name());
                break;
            }
            case SnorePluginInfo::FRONTEND:{
                Notification_Frontend * nf = qobject_cast<Notification_Frontend*> (info->load());
                qDebug() <<info->name()<<"is a Notification_Frontend";
                if(!nf->init( this )){
                    nf->deleteLater();
                    break;
                }
                m_Frontends.append(info->name());
                break;
            }
            case SnorePluginInfo::PLUGIN:{
                qDebug() <<info->name()<<"is a SnorePlugin";
                if(!info->load()->init(this)){
                    info->load()->deleteLater();
                    break;
                }
                m_plugins.append(info->name());
                break;
            }
            default:{
                std::cerr<<"Plugin Cache corrupted"<<std::endl;
                std::cerr<<info->file().toLatin1().constData()<<QString::number((int)info->type()).toLatin1().constData()<<std::endl;
            }
            }
        }else{
            qDebug()<<"dont load "<<info->file()<<info->type();
        }
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


const QStringList &SnoreServer::notificationBackends() const
{
    return m_notificationBackends;
}

const QStringList &SnoreServer::notificationFrontends() const
{
    return m_Frontends;
}

const QStringList &SnoreServer::secondaryNotificationBackends() const
{
    return m_secondaryNotificationBackends;
}

void SnoreServer::setPrimaryNotificationBackend ( const QString &backend )
{
    if(!m_notificationBackends.contains(backend))
        return;
    qDebug()<<"Setting Notification Backend to:"<<backend;
    m_notificationBackend = qobject_cast<Notification_Backend*>(pluginCache()[backend]->load());
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
