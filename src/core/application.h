#ifndef APPLICATION_H
#define APPLICATION_H
#include "snore_exports.h"
#include <QString>
#include <QStringList>
#include <QDebug>
#include <QHash>
#include <QSharedPointer>

typedef QHash<QString,QSharedPointer<class Application> > ApplicationsList ;
typedef QHash<QString,QSharedPointer<class Alert> > AlertList;
class SNORE_EXPORT Application
{
public:
    Application(const QString &name);
    Application();
    AlertList alerts;
    QString name;


    void addAlert(const QString &alert,const QString &title);
};

class SNORE_EXPORT Alert{
public:
    Alert(const QString &name,const QString &title);
    Alert(const QString &name,const QString &title,bool active);
    Alert();
    QString name;
    QString title;
    bool active;
};


#endif // APPLICATION_H
