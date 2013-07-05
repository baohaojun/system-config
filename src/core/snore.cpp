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

#include "snore.h"
#include "notification/notification.h"
#include "plugins/plugincontainer.h"
#include "plugins/plugins.h"
#include "plugins/snorebackend.h"
#include "plugins/snorefrontend.h"

#include "version.h"

#include <iostream>

#include <QPluginLoader>
#include <QDebug>
#include <QDir>
#include <QSystemTrayIcon>
#include <QApplication>
#include <QSettings>

namespace Snore{

QHash<QString,PluginContainer*> SnoreCore::s_pluginCache = QHash<QString,PluginContainer*>() ;

QDir *SnoreCore::s_pluginDir = NULL;

QSettings *SnoreCore::cacheFile(){
#ifdef Q_OS_LINUX
    return  new QSettings("TheOneRing","libsnore");
#else
    return new QSettings(SnoreCore::pluginDir().absoluteFilePath("plugin.cache"),QSettings::IniFormat);
#endif
}

void SnoreCore::slotNotificationClosed(Notification n)
{
    emit notificationClosed(n);
}

QString const SnoreCore::version(){
    static QString ver(QString().append(Version::major()).append(".").append(Version::minor()).append(Version::suffix()));
    return ver;
}

QString const SnoreCore::snoreTMP(){
    static const QString tmp = QDir::temp().path() +"/SnoreNotify/";
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

QHash<QString, PluginContainer *> SnoreCore::pluginCache(){
    if(!s_pluginCache.isEmpty())
        return s_pluginCache;
    QSettings *cache = cacheFile();
    QString version = cache->value("version").toString();
    QString path = cache->value("pluginPath").toString();
    int size = cache->beginReadArray("plugins");
    if(size == 0 || version != Version::revision() || path != pluginDir().path()){
        qDebug()<<version<<"!="<<Version::revision();
        qDebug()<<path<<"!="<<pluginDir().path();
        updatePluginCache();
    }else{
        for(int i=0;i<size;++i) {
            cache->setArrayIndex(i);
            PluginContainer::PluginType type = (PluginContainer::PluginType)cache->value("type").toInt();
            PluginContainer *info = new PluginContainer(cache->value("fileName").toString(),cache->value("name").toString(),type);
            s_pluginCache.insert(info->name(),info);
        }
        cache->endArray();
    }

    return s_pluginCache;
}

void SnoreCore::updatePluginCache(){
    QSettings *cache = cacheFile();
    qDebug()<<"Updating plugin cache"<<cache->fileName();

    s_pluginCache.clear();

    foreach(const QString &type,PluginContainer::types()){
        QDir plPath(SnoreCore::pluginDir().absoluteFilePath(type));
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
            PluginContainer *info = new PluginContainer( SnoreCore::pluginDir().relativeFilePath(filepath),sp->name(),PluginContainer::typeFromString(type));
            s_pluginCache.insert(info->name(),info);
            delete sp;
            qDebug()<<"added "<<info->name()<<"to cache";
        }
    }

    qDebug()<<s_pluginCache.keys();
    cache->setValue("version",Version::revision());
    cache->setValue("pluginPath",pluginDir().path());
    QList<PluginContainer*> plugins = s_pluginCache.values();
    cache->beginWriteArray("plugins");
    for(int i=0;i< plugins.size();++i) {
        cache->setArrayIndex(i);
        cache->setValue("fileName",plugins[i]->file());
        cache->setValue("name", plugins[i]->name());
        cache->setValue("type",(int)plugins[i]->type());
    }
    cache->endArray();
}

const QDir &SnoreCore::pluginDir(){
    if(s_pluginDir == NULL)
        setPluginDir();
    qDebug()<<"SnorePluginDir:"<<s_pluginDir->path();
    return *s_pluginDir;
}

void SnoreCore::setPluginDir(const QString &path){
    if(!path.isEmpty()){//path is not empty
        QDir *tmp = new QDir(path);
        if(tmp->exists()){//path exists
            s_pluginDir = tmp;
        }else{
            delete tmp;
        }
    }
    //if pluginpath is not initialized try snoreplugins/ in application dir
    if(s_pluginDir == NULL){
        s_pluginDir  = new QDir(qApp->applicationDirPath()+"/snoreplugins");
        qDebug()<<"PluginDir"<<s_pluginDir->absolutePath();
        if(!s_pluginDir->exists())//still not existing? use the path defined on compile time
            s_pluginDir = new QDir(LIBSNORE_PLUGIN_PATH);
    }
}

void SnoreCore::loadPlugins ( PluginContainer::PluginTypes types )
{
    qDebug()<<"PluginInfo"<<SnoreCore::pluginCache().keys();
    foreach ( PluginContainer *info, SnoreCore::pluginCache().values())
    {
        if(types == PluginContainer::ALL or  types.testFlag(info->type())){
            switch(info->type()){
            case PluginContainer::BACKEND:{
                qDebug() <<info->name()<<"is a Notification_Backend";
                m_notificationBackends.append( info->name());
                break;
            }
            case PluginContainer::SECONDARY_BACKEND:{
                SnoreSecondaryBackend *nb = qobject_cast<SnoreSecondaryBackend *> ( info->load() );
                if(!nb->init( this )){
                    nb->deleteLater();
                    break;
                }
                m_secondaryNotificationBackends.append(info->name());
                break;
            }
            case PluginContainer::FRONTEND:{
                SnoreFrontend * nf = qobject_cast<SnoreFrontend*> (info->load());
                qDebug() <<info->name()<<"is a Notification_Frontend";
                if(!nf->init( this )){
                    nf->deleteLater();
                    break;
                }
                m_Frontends.append(info->name());
                break;
            }
            case PluginContainer::PLUGIN:{
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

uint SnoreCore::broadcastNotification ( Notification notification )
{
    qDebug()<<"Broadcasting"<<notification.title()<<notification.timeout();
    emit notify ( notification );
    if ( m_notificationBackend != NULL )
    {
        if(!m_notificationBackend->isInitialized()){
            qDebug()<<"Notification backend "<<m_notificationBackend<<" isnt initialized will snore will exit now";
            qApp->quit();
        }
        notification.setId( m_notificationBackend->slotNotify( notification ));
        m_notificationBackend->addActiveNotification(notification);
        return  notification.id();
    }
    return -1;
}


void SnoreCore::notificationActionInvoked ( Notification notification )
{
    emit actionInvoked(notification);
    SnoreFrontend *nf= notification.source();
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

void SnoreCore::setPrimaryNotificationBackend ( const QString &backend )
{
    if(!pluginCache().contains(backend)){
        qDebug()<<"Unknown Backend:"<<backend;
        return;
    }
    qDebug()<<"Setting Notification Backend to:"<<backend;
    SnoreBackend* b = qobject_cast<SnoreBackend*>(pluginCache()[backend]->load());
    if(!b->isInitialized()){
        if(!b->init(this)){
            qDebug()<<"Failed to initialize"<<b->name();
            return;
        }
        connect(b,SIGNAL(closeNotification(Snore::Notification)),this,SLOT(slotNotificationClosed(Snore::Notification)));
    }


    m_notificationBackend = b;
}

const QString &SnoreCore::primaryNotificationBackend(){
    if(m_notificationBackend == NULL){
        static QString none;
        return none;
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
    if(m_notificationBackend->requestCloseNotification(n,r))
    {
        emit notificationClosed(n);
    }
}

}

#include "snore.moc"
