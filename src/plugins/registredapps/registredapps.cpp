#include "registredapps.h"
#include <QtCore>
#include <core/snoreserver.h>


Q_EXPORT_PLUGIN2(registredapps,RegistredApps)

RegistredApps::RegistredApps(){
    setProperty("name","RegistredApps");
    WebInterface::getInstance()->publicatePlugin(this);
}

bool RegistredApps::parseCommand(QTcpSocket *client, const QString &command){
    qDebug()<<"Registred apps";
    if(command.toLower()=="overview"){
        QString out;
        out+="Registred Applications\n";
        SnoreServer *snore=getSnore();
        foreach(QSharedPointer<Application> a,snore->getAplicationList()->values()){
            out+=a->name+"\n";
            out+="Registred alerts of "+a->name+"\t alert\t title \t is Active\n";
            foreach(const QSharedPointer<Alert> al,a->alerts.values())
                out+=al->name+"\t"+al->title+"\t\t"+(al->active?"true":"false")+"\n";
        }
        client->write(out.toUtf8());
        client->disconnectFromHost();
        client->waitForDisconnected();
        return true;
    }
    return false;
}

QString RegistredApps::display(){
    return "<a href=\"/overview\" >Overview of Registred applications </a><br>";
}


#include "registredapps.moc"
