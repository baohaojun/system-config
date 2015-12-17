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

#ifndef PUSHOVER_FRONTEND_H
#define PUSHOVER_FRONTEND_H
#include "libsnore/plugins/snorefrontend.h"
#include "libsnore/application.h"

#include "pushoverclient.h"

class PushoverFrontend : public Snore::SnoreFrontend
{
    Q_OBJECT
    Q_INTERFACES ( Snore::SnoreFrontend )
    Q_PLUGIN_METADATA ( IID "org.Snore.NotificationFrontend/1.0" FILE "snore_plugin.json" )
public:
    PushoverFrontend();
    ~PushoverFrontend() = default;

protected:
    void setDefaultSettings() override;

public Q_SLOTS:
    void slotActionInvoked ( Snore::Notification notification ) override;

private:
    PushoverClient* m_client;

};

#endif//PUSHOVER_FRONTEND_H
