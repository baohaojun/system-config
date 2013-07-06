/****************************************************************************************
 * Copyright (c) 2010-2012 Patrick von Reth <patrick.vonreth@gmail.com>                 *
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

#include "snarl.h"

#include "core/snore.h"
#include "core/plugins/plugins.h"
#include "core/plugins/snorebackend.h"

#include <QtCore>
#include <QtDebug>
#include <QWidget>


#include <iostream>


#define SNORENOTIFIER_MESSAGE_ID  WM_USER + 238

using namespace Snore;
using namespace Snarl::V42;

Q_EXPORT_PLUGIN2(snarl,SnarlBackend)

class SnarlBackend::SnarlWidget:public QWidget
{
    //Q_OBJECT
public:
    SnarlWidget(SnarlBackend * snarl):
        m_snarl(snarl)
    {
        SNARL_GLOBAL_MESSAGE = SnarlInterface::Broadcast();
    }

    bool winEvent( MSG * msg, long * result ){
        Q_UNUSED(result);
        if(msg->message == SNARL_GLOBAL_MESSAGE){
            int action = msg->wParam;
            if(action == SnarlEnums::SnarlLaunched){
                foreach(Application *a,m_snarl->snore()->aplications()){
                    m_snarl->slotRegisterApplication(a);
                }
            }

        }else if(msg->message == SNORENOTIFIER_MESSAGE_ID){
            int action = msg->wParam & 0xffff;
            int data = (msg->wParam & 0xffffffff) >> 16;
            uint notificationID = msg->lParam;
            Notification notification =  m_snarl->snore()->getActiveNotificationByID(notificationID);
            qDebug()<<"recived a Snarl callback id:"<<notificationID<<"action:"<<action<<"data:"<<data;
            NotificationEnums::CloseReasons::closeReasons reason = NotificationEnums::CloseReasons::NONE;
            switch(action){
            case SnarlEnums::CallbackInvoked:
                reason = NotificationEnums::CloseReasons::CLOSED;
                break;
            case SnarlEnums::NotifyAction:
                reason = NotificationEnums::CloseReasons::CLOSED;
                notification.setActionInvoked(data);
                m_snarl->snore()->notificationActionInvoked(notification);
                break;
            case SnarlEnums::CallbackClosed:
                reason = NotificationEnums::CloseReasons::DISMISSED;
                break;
            case SnarlEnums::CallbackTimedOut:
                reason = NotificationEnums::CloseReasons::TIMED_OUT;
                break;
                //away stuff
            case SnarlEnums::SnarlUserAway:
                qDebug()<<"Snalr user has gone away";
                break;
            case SnarlEnums::SnarlUserBack:
                qDebug()<<"Snalr user has returned";
                break;
            default:
                qDebug()<<"Unknown snarl action found!!";
                return false;
            }
            m_snarl->closeNotification(notification,reason);
            return true;
        }
        return false;
    }

private:
    uint SNARL_GLOBAL_MESSAGE;
    SnarlBackend* m_snarl;

};


SnarlBackend::SnarlBackend():
    SnoreBackend("Snarl"),
    m_defautSnarlinetrface(NULL)
{

}

SnarlBackend::~SnarlBackend()
{
    if(snore() != NULL){
        foreach(Application *a,snore()->aplications()){
            this->slotUnregisterApplication(a);
        }
    }
    if(m_defautSnarlinetrface)
        delete m_defautSnarlinetrface;
}


bool SnarlBackend::init(SnoreCore *snore){
    m_eventLoop = new SnarlBackend::SnarlWidget(this);
    SnarlInterface *snarlInterface = new SnarlInterface();
    m_applications.insert("SnoreNotify",snarlInterface);
    qDebug()<<"Initiating Snarl Backend, Snarl version: "<<snarlInterface->GetVersion();
    m_defautSnarlinetrface = new SnarlInterface();

    return SnoreBackend::init(snore);
}

void SnarlBackend::slotRegisterApplication(Application *application){
    SnarlInterface *snarlInterface = NULL;
    if(m_applications.contains(application->name())){
        snarlInterface = m_applications.value(application->name());
    }else{
        snarlInterface = new SnarlInterface();
        m_applications.insert(application->name(),snarlInterface);
    }
    qDebug()<<"Register with Snarl"<<application->name();
    QString appName = application->name();
    appName = appName.replace(" ","_");//app sig must not contain spaces
    snarlInterface->Register(appName.toUtf8().constData(),
                             application->name().toUtf8().constData(),
                             application->icon().localUrl().toUtf8().constData(),
                             0,m_eventLoop->winId(),SNORENOTIFIER_MESSAGE_ID);

    foreach(Alert *alert,application->alerts()){
        qDebug()<<"registering snarl alert"<<application->name();
        snarlInterface->AddClass(alert->name().toUtf8().constData(),
                                 alert->name().toUtf8().constData(),
                                 0,0,alert->icon().localUrl().toUtf8().constData());
    }
}

void SnarlBackend::slotUnregisterApplication(Application *application){
    SnarlInterface *snarlInterface = m_applications.take(application->name());
    if(snarlInterface == NULL)
        return;
    QString appName = application->name();
    appName = appName.replace(" ","_");//app sig must not contain spaces
    snarlInterface->Unregister(appName.toUtf8().constData());
    delete snarlInterface;
}

void SnarlBackend::slotNotify(Notification notification){
    SnarlInterface *snarlInterface = m_applications.value(notification.application());
    if(snarlInterface == NULL){
        qDebug()<<notification.application()<<"not in snarl interfaces, defaulting";
        qDebug()<<m_applications.keys();
        snarlInterface = m_defautSnarlinetrface;
    }

    if(!m_idMap.contains(notification.id())){
        ULONG32 id = snarlInterface->Notify(notification.alert().toUtf8().constData(),
                                            Notification::toPlainText(notification.title()).toUtf8().constData(),
                                            Notification::toPlainText(notification.text()).toUtf8().constData(),
                                            notification.timeout(),
                                            notification.icon().isLocalFile()?notification.icon().localUrl().toUtf8().constData():0,
                                            !notification.icon().isLocalFile()?notification.icon().imageData().toBase64().constData():0,
                                            notification.priority());

        foreach(const Notification::Action *a, notification.actions()){
            snarlInterface->AddAction(id,a->name.toUtf8().constData(),QString("@").append(QString::number(a->id)).toUtf8().constData());
        }
        m_idMap[notification.id()] = id;

    }else{
        //update message
        snarlInterface->Update(m_idMap[notification.id()],
                notification.alert().toUtf8().constData(),
                Notification::toPlainText(notification.title()).toUtf8().constData(),
                Notification::toPlainText(notification.text()).toUtf8().constData(),
                notification.timeout(),
                notification.icon().isLocalFile()?notification.icon().localUrl().toUtf8().constData():0,
                !notification.icon().isLocalFile()?notification.icon().imageData().toBase64().constData():0,
                notification.priority());
    }
    startTimeout(notification.id(),notification.timeout());
}

bool SnarlBackend::slotCloseNotification(Notification notification){
    m_defautSnarlinetrface->Hide(m_idMap.remove(notification.id()));
    return true;
}
