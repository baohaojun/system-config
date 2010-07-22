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

#include "snarl_backend.h"
#include <QtCore>
#include <QTextEdit>
#include <iostream>
#include <wchar.h>




Q_EXPORT_PLUGIN2(snarl_backend,Snarl_Backend)

Snarl_Backend::Snarl_Backend(SnoreServer *snore):
Notification_Backend("SnarlBackend",snore)
{
    snarlInterface=new Snarl::SnarlInterface();
    qDebug()<<"Initiating Snarl Backend, Snarl version: "<<snarlInterface->GetVersionExA();
    this->installEventFilter(this);

}
Snarl_Backend::~Snarl_Backend(){
    delete snarlInterface;
}

void Snarl_Backend::registerApplication(Application *application){
    wchar_t *appName = toWchar(application->name());
    snarlInterface->RegisterApp(appName,L"",L"");
    wprintf(L"Registering %s with Snarl.",appName);

    foreach(Alert *alert,application->alerts()){
        wchar_t *alertName = toWchar(alert->name());
        wprintf(L"Registering %s als snarl alert classSnarl.",alertName);
        snarlInterface->RegisterAlert(appName,alertName);
        delete [] alertName;
    }
    delete [] appName;
}

int Snarl_Backend::notify(QSharedPointer<Notification>notification){
    wchar_t *title = toWchar(Notification::toPlainText(notification->title()));
    wchar_t *text =  toWchar(Notification::toPlainText(notification->text()));
    wchar_t *icon =  toWchar(notification->icon());

    if(notification->id()==0){
        wprintf(L"Calling SnarlMessage\n"
                L"Title: \"%s\"\n"
                L"Text: \"%s\"\n"
                L"Timeout: \"%i\"\n"
                L"Icon: \"%s\"\n",title,text,notification->timeout(),icon);
        return snarlInterface->ShowMessage(title,text,notification->timeout(), icon);
    }else{
        //update message
        wprintf(L"Updating SnarlMessage ID: \"%i\"\n"
                L"Title: \"%s\"\n"
                L"Text: \"%s\"\n"
                L"Icon: \"%s\"\n",notification->id(),title,text,icon);
        snarlInterface->UpdateMessage(notification->id(),title, text,icon);
        return notification->id();
    }
    delete[] title;
    delete[] text;
    delete[] icon;
}

void Snarl_Backend::closeNotification(int nr){
    snarlInterface->HideMessage(nr);
}

bool Snarl_Backend::eventFilter(QObject *obj, QEvent *event){
    qDebug()<<obj->objectName();
    return true;
}

wchar_t *Snarl_Backend::toWchar(const QString &string){
    wchar_t *wc = new wchar_t[string.length()+1];
    wc[string.toWCharArray(wc)] = 0;
    return wc;
}

#include "snarl_backend.moc"
