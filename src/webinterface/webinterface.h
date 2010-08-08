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

#ifndef WEBINTERFACE_H
#define WEBINTERFACE_H
#include <QObject>
#include <QTcpServer>
#include <QHash>
#include "core/application.h"
#include "core/interface.h"

#ifdef WEBINTERFACE_DLL
# define WEBINTERFACE_EXPORT Q_DECL_EXPORT
#else
# define WEBINTERFACE_EXPORT Q_DECL_IMPORT
#endif

class WebInterface_Plugin;

class WEBINTERFACE_EXPORT WebInterface:public QObject
{
    Q_OBJECT
public:
    static const int port=9886;
    static WebInterface* getInstance();
public:
    void publicatePlugin ( WebInterface_Plugin* plugin );

public slots:
    void handleConnection();
    void handleMessages();



private:
    static WebInterface *_instance;
    WebInterface();
    ~WebInterface();
    QList<WebInterface_Plugin*> webinterfaces;
    QTcpServer *tcpServer;

    enum ARGUMENTS
    {
        ROOT,
        SUBSCRIBE,
        UNSUBSCRIBE,
        OVERVIEW
    };
    QHash<QString,WebInterface::ARGUMENTS> getArgument;
};

class WEBINTERFACE_EXPORT WebInterface_Plugin:public SnorePlugin
{
public:
    WebInterface_Plugin ( QString name,class SnoreServer *snore=0 );
    virtual ~WebInterface_Plugin();
    virtual QString display() =0;
    virtual bool parseCommand ( QTcpSocket *client,const QString &command ) =0;

};
Q_DECLARE_INTERFACE ( WebInterface_Plugin,
                      "org.Snore.WebInterface/1.0" )

#endif // WEBINTERFACE_H
