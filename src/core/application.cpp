#include "application.h"


Application::Application(const QString &name):name(name){}

Application::Application():name("Error: Uninitialized Application"){}

void Application::addAlert(const QString &alert,const QString &title){
    alerts.insert(alert,QSharedPointer<Alert>(new Alert(alert,title)));
}


Alert::Alert(const QString &name,const QString &title):name(name),title(title),active(true){}
Alert::Alert(const QString &name,const QString &title,bool active):name(name),title(title),active(active){}

Alert::Alert():active(false){}



