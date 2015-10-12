/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2015  Hannah von Reth <vonreth@kde.org>

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
#include "snarlsettings.h"

#include "libsnore/snore.h"
#include "libsnore/snore_p.h"
#include "libsnore/utils.h"
#include "libsnore/plugins/plugins.h"
#include "libsnore/plugins/snorebackend.h"
#include "libsnore/notification/notification_p.h"

#include <QWidget>

#include <iostream>

#define SNORENOTIFIER_MESSAGE_ID WM_USER + 238

using namespace Snore;
using namespace Snarl::V42;

class SnarlBackend::SnarlWidget: public QWidget
{
    //Q_OBJECT
public:
    SnarlWidget(SnarlBackend *snarl):
        m_snarl(snarl)
    {
        SNARL_GLOBAL_MESSAGE = SnarlInterface::Broadcast();
    }

    bool nativeEvent(const QByteArray &eventType, void *message, long *) override
    {
        if (eventType == "windows_generic_MSG") {
            MSG *msg = static_cast<MSG *>(message);
            if (msg->message == SNARL_GLOBAL_MESSAGE) {
                int action = msg->wParam;
                if (action == SnarlEnums::SnarlLaunched) {
                    for (const Application &a : SnoreCore::instance().aplications()) {
                        m_snarl->slotRegisterApplication(a);
                    }
                }

            } else if (msg->message == SNORENOTIFIER_MESSAGE_ID) {
                int action = msg->wParam & 0xffff;
                int data = (msg->wParam & 0xffffffff) >> 16;

                Notification notification;
                if (msg->lParam != 0) {
                    notification =  m_snarl->m_idMap.value(msg->lParam);
                }

                Notification::CloseReasons reason = Notification::NONE;
                switch (action) {
                case SnarlEnums::CallbackInvoked:
                    reason = Notification::ACTIVATED;
                    snoreDebug(SNORE_DEBUG) << "Notification clicked";
                    break;
                case SnarlEnums::NotifyAction:
                    reason = Notification::ACTIVATED;
                    snoreDebug(SNORE_DEBUG) << "Notification action invoked";
                    if (notification.isValid()) {
                        m_snarl->slotNotificationActionInvoked(notification, notification.actions().value(data));
                    }
                    break;
                case SnarlEnums::CallbackClosed:
                    reason = Notification::DISMISSED;
                    snoreDebug(SNORE_DEBUG) << "Notification dismissed";
                    break;
                case SnarlEnums::CallbackTimedOut:
                    reason = Notification::TIMED_OUT;
                    snoreDebug(SNORE_DEBUG) << "Notification timed out";
                    break;
                //away stuff
                case SnarlEnums::SnarlUserAway:
                    snoreDebug(SNORE_DEBUG) << "Snalr user has gone away";
                    return true;
                case SnarlEnums::SnarlUserBack:
                    snoreDebug(SNORE_DEBUG) << "Snalr user has returned";
                    return true;
                default:
                    snoreDebug(SNORE_WARNING) << "Unknown snarl action found!!";
                    return false;
                }
                if (notification.isValid()) {
                    m_snarl->requestCloseNotification(notification, reason);
                    m_snarl->m_idMap.take(msg->lParam);
                } else {
                    snoreDebug(SNORE_WARNING) << "Snarl notification already closed" << msg->lParam << action;
                    snoreDebug(SNORE_WARNING) << m_snarl->m_idMap;
                }
                return true;
            }
        }
        return false;
    }

private:
    uint SNARL_GLOBAL_MESSAGE;
    SnarlBackend *m_snarl;

};

SnarlBackend::SnarlBackend():
    m_eventLoop(new SnarlBackend::SnarlWidget(this))
{

}

SnarlBackend::~SnarlBackend()
{
    delete m_eventLoop;
}

PluginSettingsWidget *SnarlBackend::settingsWidget()
{
    return new SnarlSettings(this);
}

bool SnarlBackend::canCloseNotification() const
{
    return true;
}

bool SnarlBackend::canUpdateNotification() const
{
    return true;
}

bool SnarlBackend::isReady()
{
    bool running = SnarlInterface::IsSnarlRunning();
    if (!running) {
        setErrorString(tr("%1 is not running.").arg(name()));
    }
    return running;
}

void SnarlBackend::setDefaultSettings()
{

    setDefaultSettingsValue(QLatin1String("Password"), QString());
    SnoreBackend::setDefaultSettings();
}

void SnarlBackend::slotRegisterApplication(const Application &application)
{
    if (!m_eventLoop) {
        return;
    }

    Q_ASSERT_X(!m_applications.contains(application.name()), Q_FUNC_INFO, "Application already registered");

    SnarlInterface *snarlInterface = new SnarlInterface();
    m_applications.insert(application.name(), snarlInterface);

    QString appName = application.name().replace(QLatin1Char(' '), QLatin1Char('_')); //app sig must not contain spaces
    QString password = settingsValue(QLatin1String("Password")).toString();
    LONG32 result = snarlInterface->Register(appName.toUtf8().constData(),
                    application.name().toUtf8().constData(),
                    application.icon().localUrl(QSize(128, 128)).toUtf8().constData(),
                    password.isEmpty() ? 0 : password.toUtf8().constData(),
                    (HWND)m_eventLoop->winId(), SNORENOTIFIER_MESSAGE_ID);
    snoreDebug(SNORE_DEBUG) << result;

    foreach(const Alert & alert, application.alerts()) {
        snarlInterface->AddClass(alert.name().toUtf8().constData(),
                                 alert.name().toUtf8().constData(),
                                 0, 0, alert.icon().localUrl(QSize(128, 128)).toUtf8().constData());
    }
}

void SnarlBackend::slotDeregisterApplication(const Application &application)
{
    if (!m_applications.contains(application.name())) {
        snoreDebug(SNORE_DEBUG) << "Unknown apllication: " << application.name();
        return;
    }
    SnarlInterface *snarlInterface = m_applications.take(application.name());
    QString appName = application.name().replace(QLatin1Char(' '), QLatin1Char('_')); //app sig must not contain spaces
    snarlInterface->Unregister(appName.toUtf8().constData());
    delete snarlInterface;
}

void SnarlBackend::slotNotify(Notification notification)
{
    if (!m_applications.contains(notification.application().name())) {
        snoreDebug(SNORE_DEBUG) << "Unknown apllication: " << notification.application().name();
        return;
    }

    SnarlInterface *snarlInterface = m_applications.value(notification.application().name());

    Snarl::V42::SnarlEnums::MessagePriority priority = Snarl::V42::SnarlEnums::PriorityUndefined;
    if (notification.priority() > 1) {
        priority = Snarl::V42::SnarlEnums::PriorityHigh;
    } else if (notification.priority() < -1) {
        priority = Snarl::V42::SnarlEnums::PriorityLow;
    } else {
        priority = static_cast<Snarl::V42::SnarlEnums::MessagePriority>(notification.priority());
    }

    ULONG32 id = 0;
    snoreDebug(SNORE_DEBUG) << notification.icon();

    if (!notification.isUpdate()) {
        id = snarlInterface->Notify(notification.alert().name().toUtf8().constData(),
                                    notification.title().toUtf8().constData(),
                                    notification.text().toUtf8().constData(),
                                    notification.timeout(),
                                    nullptr,
                                    Utils::dataFromImage(notification.icon().pixmap(QSize(128, 128)).toImage()).toBase64().constData(),
                                    priority);

        foreach(const Action & a, notification.actions()) {
            snarlInterface->AddAction(id, a.name().toUtf8().constData(), (QLatin1Char('@') + QString::number(a.id())).toUtf8().constData());
        }
        m_idMap[id] = notification;
        notification.hints().setPrivateValue(this, "id", id);
    } else {
        //update message
        id = notification.old().hints().privateValue(this, "id").toUInt();
        snarlInterface->Update(id,
                               notification.alert().name().toUtf8().constData(),
                               notification.title().toUtf8().constData(),
                               notification.text().toUtf8().constData(),
                               notification.timeout(),
                               nullptr,
                               Utils::dataFromImage(notification.icon().pixmap(QSize(128, 128)).toImage()).toBase64().constData(),
                               priority);
    }

    notification.hints().setPrivateValue(this, "id", id);
    slotNotificationDisplayed(notification);//if dnd or away snarl does not timeout atomatically

}

void SnarlBackend::slotCloseNotification(Notification notification)
{
    if (!m_applications.contains(notification.application().name())) {
        snoreDebug(SNORE_DEBUG) << "Unknown apllication: " << notification.application().name();
        return;
    }
    ULONG32 id = notification.hints().privateValue(this, "id").toUInt();
    m_idMap.remove(id);
    m_applications.value(notification.application().name())->Hide(id);
}
