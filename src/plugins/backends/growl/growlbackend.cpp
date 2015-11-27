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

#include "growlbackend.h"
#include "growlsettings.h"

#include "libsnore/snore.h"
#include "libsnore/snore_p.h"
#include "libsnore/utils.h"

#include <functional>

using namespace Snore;

GrowlBackend *GrowlBackend::s_instance = nullptr;

GrowlBackend::GrowlBackend()
{
    s_instance = this;

    auto func = [](growl_callback_data * data)->void {
        snoreDebug(SNORE_DEBUG) << data->id << QString::fromUtf8(data->reason) << QString::fromUtf8(data->data);
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
    Growl::init((GROWL_CALLBACK)static_cast<void(*)(growl_callback_data *)>(func));
}

GrowlBackend::~GrowlBackend()
{
    Growl::shutdown();
}

bool GrowlBackend::isReady()
{
    bool running = Growl::isRunning(GROWL_TCP, settingsValue(QStringLiteral("Host")).toString().toUtf8().constData());
    if (!running) {
        setErrorString(tr("%1 is not running.").arg(name()));
    }
    return running;
}

void GrowlBackend::slotRegisterApplication(const Application &application)
{
    snoreDebug(SNORE_DEBUG) << application.name();
    std::vector<std::string> alerts;
    foreach(const Alert & a, application.alerts()) {
        snoreDebug(SNORE_DEBUG) << a.name();
        alerts.push_back(a.name().toUtf8().constData());
    }

    Growl *growl = new Growl(GROWL_TCP, settingsValue(QStringLiteral("Host")).toString().toUtf8().constData(),
                             settingsValue(QStringLiteral("Password")).toString().toUtf8().constData(),
                             application.name().toUtf8().constData());
    m_applications.insert(application.name(), growl);
    growl->Register(alerts, application.icon().localUrl(QSize(128, 128)).toUtf8().constData());

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

    data.setIcon(notification.icon().localUrl(QSize(128, 128)).toUtf8().constData());
    data.setCallbackData("1");
    growl->Notify(data);

    slotNotificationDisplayed(notification);
}

PluginSettingsWidget *GrowlBackend::settingsWidget()
{
    return new GrowlSettings(this);
}

void GrowlBackend::setDefaultSettings()
{
    SnoreBackend::setDefaultSettings();
    setDefaultSettingsValue(QStringLiteral("Host"), QLatin1String("localhost"));
    setDefaultSettingsValue(QStringLiteral("Password"), QString());
}

