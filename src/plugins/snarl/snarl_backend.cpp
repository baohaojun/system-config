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

int Snarl_Backend::notify(QSharedPointer<Notification>notification){
    QString qtitle(Notification::toPlainText(notification->title()));
    QString qtext( Notification::toPlainText(notification->text()));
    QString qicon(notification->icon());

    wchar_t *title = new wchar_t[qtitle.length()+1];
    wchar_t *text =  new wchar_t[qtext.length()+1];
    wchar_t *icon =  new wchar_t[qicon.length()+1];

    int i=0;
    i=qtitle.toWCharArray(title);
    title[i+1]=0;
    i=qtext.toWCharArray(text);
    text[i+1]=0;
    i=qicon.toWCharArray(icon);
    icon[i+1]=0;

    if(notification->id()==0){
        wprintf(L"Calling SnarlMessage\n"
                L"Title: %s\n"
                L"Text:%s\n"
                L"Timeout: %i\n"
                L"Icon: %s\n",title,text,notification->timeout(),icon);
        return snarlInterface->ShowMessage(title,text,notification->timeout(), icon);
    }else{
        //update message
        wprintf(L"Updating SnarlMessage ID: %i\n"
                L"Title: %s\n"
                L"Text:%s\n"
                L"Icon: %s\n",notification->id(),title,text,icon);
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


#include "snarl_backend.moc"
