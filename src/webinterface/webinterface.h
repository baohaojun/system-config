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

class WEBINTERFACE_EXPORT WebInterface_Plugin:public SnorePlugin{
public:
    virtual ~WebInterface_Plugin() {}
    virtual QString display()=0;
    virtual bool parseCommand(QTcpSocket *client,const QString &command)=0;

};
Q_DECLARE_INTERFACE(WebInterface_Plugin,
                    "org.Snore.WebInterface/1.0")

class WEBINTERFACE_EXPORT WebInterface:public QObject
{
    Q_OBJECT
public:
    static const int port=9886;
    static WebInterface* getInstance();
public:
     void publicatePlugin(WebInterface_Plugin* plugin);

public slots:
    void handleConnection();
    void handleMessages();



private:
    static QPointer<WebInterface> instance;
    WebInterface();
    ~WebInterface();
    QList<WebInterface_Plugin*> webinterfaces;
    QTcpServer *tcpServer;

    enum ARGUMENTS{
        ROOT,
        SUBSCRIBE,
        UNSUBSCRIBE,
        OVERVIEW
    };
    QHash<QString,WebInterface::ARGUMENTS> getArgument;
};

#endif // WEBINTERFACE_H
