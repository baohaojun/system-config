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

#ifndef SNORE_PLUGINS_H
#define SNORE_PLUGINS_H
#include "../snore_exports.h"
#include "../notification/notification.h"

#include <QPointer>
#include <QFlag>

namespace Snore{
class Application;
class SnoreCore;


class SNORE_EXPORT SnorePlugin:public QObject
{
    Q_OBJECT
public:
    SnorePlugin ( const QString &name);
    virtual ~SnorePlugin();
    virtual bool init( SnoreCore* snore );
    bool isInitialized();
    SnoreCore* snore();
    const QString &name() const;

protected:
    void startTimeout(uint id,int timeout);
private slots:
    void notificationTimedOut();

private:
    SnorePlugin() {}
    QString m_name;
    bool m_initialized;
    QPointer<SnoreCore> m_snore;
    QHash<uint,QTimer*> m_timeouts;
    QList<uint> m_timeout_order;


};

}
Q_DECLARE_INTERFACE ( Snore::SnorePlugin,
                      "org.Snore.SnorePlugin/1.0" )




#endif//SNORE_PLUGINS_H
