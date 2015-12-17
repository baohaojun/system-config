/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2015  Hannah von Reth <vonreth@kde.org>

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

#ifndef SNORESECONDARYBACKEND_H
#define SNORESECONDARYBACKEND_H

#include "libsnore/snore_exports.h"
#include "libsnore/plugins/plugins.h"

namespace Snore
{
class SnoreCore;

class SNORE_EXPORT SnoreSecondaryBackend : public SnorePlugin
{
    Q_OBJECT
    Q_INTERFACES ( Snore::SnorePlugin Snore::SnorePlugin )
public:
    SnoreSecondaryBackend();
    virtual ~SnoreSecondaryBackend();

    PluginTypes type() const override {
        return SecondaryBackend;
    }

public Q_SLOTS:
    virtual void slotNotify ( Snore::Notification notification );
    virtual void slotNotificationDisplayed ( Snore::Notification notification );

};

}

Q_DECLARE_INTERFACE ( Snore::SnoreSecondaryBackend,
                      "org.Snore.SecondaryNotificationBackend/1.0" )

#endif // SNORESECONDARYBACKEND_H
