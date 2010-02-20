#ifndef REGISTREDAPPS_H
#define REGISTREDAPPS_H
#include "core/snore_exports.h"
#include "core/interface.h"
#include "webinterface/webinterface.h"
#include <QtNetwork>

class RegistredApps:public QObject,WebInterface_Plugin{
    Q_OBJECT
    Q_INTERFACES(SnorePlugin WebInterface_Plugin)
public:
    RegistredApps();
    bool parseCommand(QTcpSocket *client, const QString &command);
    QString display();

};
#endif//REGISTREDAPPS_H
