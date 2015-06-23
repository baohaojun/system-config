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

#include "growlbackend.h"
#include "growlsettings.h"

#include "libsnore/snore.h"
#include "libsnore/snore_p.h"
#include "libsnore/utils.h"

#include <functional>

using namespace Snore;

GrowlBackend *GrowlBackend::s_instance = nullptr;

GrowlBackend::GrowlBackend():
    SnoreBackend("Growl", false, false)
{
    setDefaultValue("Host", "localhost");
    setDefaultValue("Password", "");
}

GrowlBackend::~GrowlBackend()
{
}

bool GrowlBackend::initialize()
{
    s_instance = this;
    auto func = [](growl_callback_data * data)->void {
        snoreDebug(SNORE_DEBUG) << data->id << QString(data->reason) << QString(data->data);
        Notification n = Snore::SnoreCore::instance().getActiveNotificationByID(data->id);
        if (!n.isValid())
        {
            return;
        }
        Notification::CloseReasons r = Notification::NONE;
        std::string reason(data->reason);
        if (reason == "TIMEDOUT")
        {
            r = Notification::TIMED_OUT;
        } else if (reason == "CLOSED")
        {
            r = Notification::DISMISSED;
        } else if (reason == "CLICK")
        {
            r = Notification::ACTIVATED;
            s_instance->slotNotificationActionInvoked(n);
        }
        s_instance->closeNotification(n, r);
    };
    if (Growl::init((GROWL_CALLBACK)static_cast<void(*)(growl_callback_data *)>(func))
            && Growl::isRunning(GROWL_TCP, value("Host").toString().toUtf8().constData())) {
        return SnoreBackend::initialize();
    }
    snoreDebug(SNORE_DEBUG) << "Growl is not running";
    return false;
}

bool GrowlBackend::deinitialize()
{
    if (!Growl::shutdown()) {
        return false;
    }
    return SnoreBackend::deinitialize();
}

void GrowlBackend::slotRegisterApplication(const Application &application)
{
    //    snoreDebug( SNORE_DEBUG ) << application.name().toUtf8().constData();
    std::vector<std::string> alerts;
    foreach(const Alert & a, application.alerts()) {
        snoreDebug(SNORE_DEBUG) << a.name().toUtf8().constData();
        alerts.push_back(a.name().toUtf8().constData());
    }

    Growl *growl = new Growl(GROWL_TCP, value("Host").toString().toUtf8().constData(),
                             value("Password").toString().toUtf8().constData(),
                             application.name().toUtf8().constData());
    growl->Register(alerts, application.icon().localUrl().toUtf8().constData());

    m_applications.insert(application.name(), growl);
}

void GrowlBackend::slotDeregisterApplication(const Application &application)
{
    Growl *growl = m_applications.take(application.name());
    if (growl == nullptr) {
        return;
    }
    delete growl;
}

void GrowlBackend::slotNotify(Notification notification)
{
    Growl *growl = m_applications.value(notification.application().name());
    QString alert = notification.alert().name();
    snoreDebug(SNORE_DEBUG) << "Notify Growl:" << notification.application() << alert << notification.title();

    GrowlNotificationData data(alert.toUtf8().constData(), notification.id(),
                               notification.title().toUtf8().constData(),
                               notification.text().toUtf8().constData());

    if (notification.icon().isValid()) {
        data.setIcon(notification.icon().localUrl().toUtf8().constData());
    }
    data.setCallbackData("1");
    growl->Notify(data);

    slotNotificationDisplayed(notification);
}

PluginSettingsWidget *GrowlBackend::settingsWidget()
{
    return new GrowlSettings(this);
}

