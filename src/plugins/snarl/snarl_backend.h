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

#ifndef SNARL_BACKEND_H
#define SNARL_BACKEND_H
#include "core/plugins/snorebackend.h"
#include "SnarlInterface.h"

#include <QWidget>

namespace Snore{
    class SnoreServer;
}

class SnarlWidget;

class Snarl_Backend:public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    friend class SnarlWidget;
public:
    Snarl_Backend();
    ~Snarl_Backend();
    virtual bool init(Snore::SnoreServer *snore);

private:
    SnarlWidget* winIDWidget;
    QHash<QString,Snarl::V42::SnarlInterface*> _applications;
    Snarl::V42::SnarlInterface* m_defautSnarlinetrface;
    bool _away;

public slots:
    void registerApplication(Snore::Application *application);
    void unregisterApplication(Snore::Application *application);
    uint notify(Snore::Notification notification);
    void closeNotification(Snore::Notification notification);

};

class SnarlWidget:public QWidget
{
    Q_OBJECT
public:
    SnarlWidget(Snarl_Backend* snarl);
    bool winEvent( MSG * message, long * result );

private:
    uint SNARL_GLOBAL_MESSAGE;
    Snarl_Backend* _snarl;

};



#endif // SNARL_BACKEND_H
