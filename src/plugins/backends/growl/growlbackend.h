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

#ifndef GROWL_BACKEND_H
#define GROWL_BACKEND_H
#include "libsnore/plugins/snorebackend.h"

#include "growl.hpp"
#include <string>

class GrowlBackend: public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0" FILE "snore_plugin.json")

public:
    GrowlBackend();
    ~GrowlBackend();
    bool isReady() override;

protected:
    void setDefaultSettings() override;

private:
    //a static instance for the static callback methode
    static GrowlBackend *s_instance;
    QHash<QString, Growl *> m_applications;

public Q_SLOTS:
    void slotRegisterApplication(const Snore::Application &application) override;
    void slotDeregisterApplication(const Snore::Application &application) override;
    void slotNotify(Snore::Notification notification) override;

};

#endif // GROWL_BACKEND_H
