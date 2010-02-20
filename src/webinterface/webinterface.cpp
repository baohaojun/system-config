#include "webinterface.h"
#include "core/snoreserver.h"
#include <QTcpServer>
#include <QObject>
#include <QTcpSocket>
#include  <QDebug>
#include <QtCore>

QPointer<WebInterface> WebInterface::instance=0;

WebInterface* WebInterface::getInstance(){
    if(instance.isNull())
        instance=new WebInterface();
    return instance.data();
}

WebInterface::WebInterface()
{
    tcpServer=new QTcpServer(this);
    if(!tcpServer->listen(QHostAddress::Any,port)){
        qDebug()<<"The subscription port is already used";
    }
    getArgument.insert("",ROOT);
    getArgument.insert("overview",OVERVIEW);
    connect(tcpServer, SIGNAL(newConnection()), this, SLOT(handleConnection()));

}
WebInterface::~WebInterface(){
    qDebug()<<"Unloading Webinterface";
    tcpServer->deleteLater();
}

void WebInterface::publicatePlugin(WebInterface_Plugin *plugin){
    webinterfaces.append(plugin);
    qDebug()<<"appending Webinterface Plugin";
}

void WebInterface::handleConnection(){
    QTcpSocket* client= tcpServer->nextPendingConnection();
    connect(client,SIGNAL(readyRead()),this,SLOT(handleMessages()));
}

void WebInterface::handleMessages(){
    qDebug()<<"Webinteface";
    QTcpSocket *client= (QTcpSocket*)sender();
    QString in(QString::fromUtf8( client->readAll()));
    in=in.mid(in.indexOf("/")+1);
    in=in.mid(0,in.indexOf(" "));
    QString out;
    qDebug()<<getArgument.value(in.toLower());
    if(!in.isEmpty())
        foreach(WebInterface_Plugin* pl,webinterfaces){
        qDebug()<<"Paring in:";
        if(pl->parseCommand(client,in.toLower()))
            return;
    }
    out+="<html><head><TITLE>SnoreNotify Configuration</TITLE></head><body>";
    foreach(WebInterface_Plugin* plugin, webinterfaces)
        out.append(plugin->display());
    out+="</body></html>";
    client->write(out.toUtf8());
    qDebug()<<"Sending over web interface:\n"<<out;
    client->disconnectFromHost();
    client->waitForDisconnected();

}



#include "webinterface.moc"
