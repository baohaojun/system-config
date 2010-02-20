#ifndef DBUSBINDING_H
#define DBUSBINDING_H
#include <QtDBus>
#include "core/interface.h"
#include "core/application.h"
#include "core/snoreserver.h"

class DBusPlugin:public QObject,SnorePlugin{
    Q_OBJECT
    Q_INTERFACES(SnorePlugin)
public:
            DBusPlugin(){
        setProperty("name","DbusBinding");
    };
    void setSnore(SnoreServer *snore);

};

class DBusBinding: public QDBusAbstractAdaptor{
    Q_OBJECT
    Q_CLASSINFO("D-Bus Interface", "org.SnoreNotify")
public:
            DBusBinding(DBusPlugin* db,SnoreServer* snore);
    ~DBusBinding();
    static void registerTypes();
private:
    QPointer<SnoreServer> snore;

public slots:
    ApplicationsList getApplicationList();
    void setAlertActive(const QString &application,const QString &alert,const bool active);

signals:
    void applicationListChanged(const ApplicationsList &);

private slots:
    void applicationListChangedSlot();

};


Q_DECLARE_METATYPE(ApplicationsList);
QDBusArgument &operator<<(QDBusArgument &a, const ApplicationsList &ap);
const QDBusArgument & operator >>(const QDBusArgument &a, ApplicationsList  &ap) ;

Q_DECLARE_METATYPE(Application);
QDBusArgument &operator<<(QDBusArgument &a, const Application &ap);
const QDBusArgument & operator >>(const QDBusArgument &a, Application  &ap) ;

Q_DECLARE_METATYPE(Alert);
QDBusArgument &operator<<(QDBusArgument &a, const Alert &al);
const QDBusArgument & operator >>(const QDBusArgument &a, Alert &al) ;

Q_DECLARE_METATYPE(AlertList);
QDBusArgument &operator<<(QDBusArgument &a, const AlertList &al);
const QDBusArgument & operator >>(const QDBusArgument &a, AlertList &al) ;






#endif // DBUSBINDING_H
