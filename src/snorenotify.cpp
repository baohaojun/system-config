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

#include "snorenotify.h"
#include "trayicon.h"
#include "core/snoreserver.h"

#include <QDir>
#include <QFile>
#include <QList>
#include <QDebug>
#include <QPluginLoader>
#include <QSystemTrayIcon>
#include <QtXml/qdom.h>
#include <QFile>
#include <QTextStream>

#include <iostream>

#ifdef Q_WS_WIN
	#define PROFILEPATH qgetenv("APPDATA") + "\\snorenotify\\"
#elif defined(Q_OS_LINUX)
	#define PROFILEPATH qgetenv("HOME") + "/.snorenotify/"
#endif
	



SnoreNotify::SnoreNotify()
{
	_trayIcon = new TrayIcon();
    _snore = new SnoreServer(_trayIcon->trayIcon());
    _snore->cleanupTMP();

    QDir pluginsDir ( qApp->applicationDirPath() +"/snoreplugins" );
    foreach ( QString fileName, pluginsDir.entryList ( QDir::Files ) )
    {
        _snore->publicatePlugin ( pluginsDir.absoluteFilePath ( fileName ) );
    }


    _trayIcon->initConextMenu(_snore);

	load();
	connect(qApp,SIGNAL(aboutToQuit()),this,SLOT(exit()));
}

void SnoreNotify::load(){
	QDomDocument doc( "Settings" );
    QFile file( PROFILEPATH + "settings.xml" );
    if(!file.open( QFile::ReadOnly )){
        std::cout<<"Failed openeing settings file: "<<QString(PROFILEPATH).toLatin1().data()<<"settings.xml"<<std::endl;
        return;
    }

    QString errorMessage;
    int errorLine = -1;
    int errorCulumn = -1;

    if(!doc.setContent(&file,&errorMessage,&errorLine, &errorCulumn )){
        std::cout<<"Failed parsing settings file: "<<QString(PROFILEPATH).toLatin1().data()<<"settings.xml\n"
                <<"Error: "<<errorMessage.toLatin1().data()<<" line: "
                <<QString::number(errorLine).toLatin1().data()<<" collumn: "<<QString::number(errorCulumn).toLatin1().data()<<std::endl;
        file.close();
        return;
    }
    file.close();

    QDomElement root = doc.documentElement();
    QDomElement backend = root.elementsByTagName("notificationBackend").item(0).toElement();
	if(!backend.isNull())        
		_snore->setPrimaryNotificationBackend(_snore->primaryNotificationBackends().value(backend.text()));
}

void SnoreNotify::save(){
	 QDomDocument doc("Settings");
    QDomElement root = doc.createElement( "SnoreNotifyProfile" );
    doc.appendChild(root);
    QDomElement backend = doc.createElement( "notificationBackend");
    backend.appendChild(doc.createTextNode(_snore->primaryNotificationBackend()->name()));
    root.appendChild(backend);


    QFile file( PROFILEPATH + "settings.xml" );
    if(!file.exists()){
        QDir(PROFILEPATH).mkpath(PROFILEPATH);
    }
    file.open(QFile::WriteOnly);

    QTextStream ts( &file );
    ts << doc.toString();
    file.close();
}

void SnoreNotify::exit(){
	qDebug()<<"Saving snore settings";
	save();
	_trayIcon->hide();

}


#include "snorenotify.moc"
