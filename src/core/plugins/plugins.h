/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013  Patrick von Reth <vonreth@kde.org>


    VSD is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    VSD is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with SnarlNetworkBridge.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef SNORE_PLUGINS_H
#define SNORE_PLUGINS_H
#include "../snore_exports.h"

#include <QPointer>
#include <QHash>
#include <QTimer>

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
