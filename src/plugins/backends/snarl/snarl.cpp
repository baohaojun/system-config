/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2014  Patrick von Reth <vonreth@kde.org>


    SnoreNotify is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SnoreNotify is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with SnoreNotify.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "snarl.h"

#include "core/snore.h"
#include "core/snore_p.h"
#include "core/plugins/plugins.h"
#include "core/plugins/snorebackend.h"
#include "core/notification/notification_p.h"

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
                foreach(const Application &a,m_snarl->snore()->aplications())
                {
                    m_snarl->slotRegisterApplication(a);
                }
            }

        }else if(msg->message == SNORENOTIFIER_MESSAGE_ID){
            int action = msg->wParam & 0xffff;
            int data = (msg->wParam & 0xffffffff) >> 16;

            Notification notification;
            if(msg->lParam != 0)
            {
                uint notificationID = 0;
                for(QHash<uint,LONG32>::iterator it = m_snarl->m_idMap.begin();it != m_snarl->m_idMap.end();++it)
                {
                    if(it.value() == msg->lParam)
                    {
                        notificationID = it.key();
                        break;
                    }
                }
                notification =  m_snarl->snore()->getActiveNotificationByID(notificationID);
                qDebug()<<"recived a Snarl callback id:"<<notificationID<< "|" << msg->lParam <<"action:"<<action<<"data:"<<data;
            }

            NotificationEnums::CloseReasons::closeReasons reason = NotificationEnums::CloseReasons::NONE;
            switch(action){
            case SnarlEnums::CallbackInvoked:
                reason = NotificationEnums::CloseReasons::CLOSED;
                break;
            case SnarlEnums::NotifyAction:
                reason = NotificationEnums::CloseReasons::CLOSED;
                if(notification.isValid())
                {
                    notification.data()->setActionInvoked(data);
                    m_snarl->snore()->d()->notificationActionInvoked(notification);
                }
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
                return true;
            case SnarlEnums::SnarlUserBack:
                qDebug()<<"Snalr user has returned";
                return true;
            default:
                qDebug()<<"Unknown snarl action found!!";
                return false;
            }
            if(notification.isValid())
            {
                m_snarl->closeNotification(notification,reason);
            }
            else
            {
                qDebug() << "Snarl notification already closed" << msg->lParam << action;
                qDebug() << m_snarl->m_idMap;
            }
            return true;
        }
        return false;
    }

private:
    uint SNARL_GLOBAL_MESSAGE;
    SnarlBackend* m_snarl;

};


SnarlBackend::SnarlBackend():
    SnoreBackend("Snarl",true,false)
{

}

SnarlBackend::~SnarlBackend()
{

}


bool SnarlBackend::initialize(SnoreCore *snore)
{
    SnarlInterface *snarlInterface = new SnarlInterface();
    if(!snarlInterface->IsSnarlRunning())
    {
        delete snarlInterface;
        return false;
    }
    m_eventLoop = new SnarlBackend::SnarlWidget(this);
    m_applications.insert(snore->d()->defaultApplication().name(),snarlInterface);
    qDebug() << "Initiating Snarl Backend, Snarl version: " << snarlInterface->GetVersion();

    return SnoreBackend::initialize(snore);
}

bool SnarlBackend::deinitialize()
{
    if(SnoreBackend::deinitialize())
    {
        if(m_eventLoop)
        {
            m_eventLoop->deleteLater();
            m_eventLoop = NULL;
        }
        m_applications.clear();
        return true;
    }
    return false;
}

void SnarlBackend::slotRegisterApplication(const Application &application){
    SnarlInterface *snarlInterface = NULL;
    if(m_applications.contains(application.name()))
    {
        snarlInterface = m_applications.value(application.name());
    }
    else
    {
        snarlInterface = new SnarlInterface();
        m_applications.insert(application.name(),snarlInterface);
    }
    QString appName = application.name().replace(" ","_");//app sig must not contain spaces
    snarlInterface->Register(appName.toUtf8().constData(),
                             application.name().toUtf8().constData(),
                             application.icon().localUrl().toUtf8().constData(),
                             0,(HWND)m_eventLoop->winId(),SNORENOTIFIER_MESSAGE_ID);

    foreach(const Alert &alert,application.alerts())
    {
        snarlInterface->AddClass(alert.name().toUtf8().constData(),
                                 alert.name().toUtf8().constData(),
                                 0,0,alert.icon().localUrl().toUtf8().constData());
    }
}

void SnarlBackend::slotDeregisterApplication(const Application &application){
    SnarlInterface *snarlInterface = m_applications.take(application.name());
    if(snarlInterface == NULL)
    {
        return;
    }
    QString appName = application.name().replace(" ","_");//app sig must not contain spaces
    snarlInterface->Unregister(appName.toUtf8().constData());
    delete snarlInterface;
}

void SnarlBackend::slotNotify(Notification notification){
    SnarlInterface *snarlInterface = m_applications.value(notification.application().name());
    if(snarlInterface == NULL)
    {
        qDebug()<<notification.application()<<"not in snarl interfaces, defaulting";
        qDebug()<<m_applications.keys();
        snarlInterface = m_applications[snore()->d()->defaultApplication().name()];
    }

    Snarl::V42::SnarlEnums::MessagePriority priority = Snarl::V42::SnarlEnums::PriorityUndefined;
    switch(notification.priority())
    {
    case NotificationEnums::Prioritys::LOW:
        priority = Snarl::V42::SnarlEnums::PriorityLow;
        break;
    case NotificationEnums::Prioritys::NORMAL:
        priority = Snarl::V42::SnarlEnums::PriorityNormal;
        break;
    case NotificationEnums::Prioritys::HIGH:
        priority = Snarl::V42::SnarlEnums::PriorityHigh;
        break;
    }

    if(notification.updateID() == 0)
    {
        ULONG32 id = snarlInterface->Notify(notification.alert().name().toUtf8().constData(),
                                            Snore::toPlainText(notification.title()).toUtf8().constData(),
                                            Snore::toPlainText(notification.text()).toUtf8().constData(),
                                            notification.timeout(),
                                            notification.icon().isLocalFile()?notification.icon().localUrl().toUtf8().constData():0,
                                            !notification.icon().isLocalFile()?notification.icon().imageData().toBase64().constData():0,
                                            priority);

        foreach(const Action &a, notification.actions())
        {
            snarlInterface->AddAction(id,a.name().toUtf8().constData(),QString("@").append(QString::number(a.id())).toUtf8().constData());
        }
        m_idMap[notification.id()] = id;
        qDebug() << "snarl" << id << notification.id();
        startTimeout(notification.id(),notification.timeout());

    }
    else
    {
        //update message
        snarlInterface->Update(m_idMap[notification.updateID()],
                notification.alert().name().toUtf8().constData(),
                Snore::toPlainText(notification.title()).toUtf8().constData(),
                Snore::toPlainText(notification.text()).toUtf8().constData(),
                notification.timeout(),
                notification.icon().isLocalFile()?notification.icon().localUrl().toUtf8().constData():0,
                !notification.icon().isLocalFile()?notification.icon().imageData().toBase64().constData():0,
                priority);
        m_idMap[notification.id()] = m_idMap[notification.updateID()];
        qDebug() << "snarl update" << m_idMap[notification.updateID()] << notification.id();
        startTimeout(notification.updateID(),notification.timeout());
    }

}

void SnarlBackend::slotCloseNotification(Notification notification)
{
    SnarlInterface *snarlInterface = m_applications.value(notification.application().name());
    if(snarlInterface == NULL)
    {
        qDebug()<<notification.application()<<"not in snarl interfaces, defaulting";
        qDebug()<<m_applications.keys();
        snarlInterface = m_applications[snore()->d()->defaultApplication().name()];
    }
    snarlInterface->Hide(m_idMap.take(notification.id()));
}
