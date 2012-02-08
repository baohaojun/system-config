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

#ifndef PLUGINCONTAINER_H
#define PLUGINCONTAINER_H
#include "../snore_exports.h"

#include <QPointer>
#include <QFlag>

namespace Snore{
class SnoreCore;
class SnorePlugin;
class SnoreFrontend;
class SnoreBackend;
class SnoreSecondaryBackend;

class SNORE_EXPORT PluginContainer{
public:
    enum PluginType{
        ALL = 0x0,//for loading plugins
        BACKEND = 0x1,
        SECONDARY_BACKEND = 0x2,
        FRONTEND = 0x4,
        PLUGIN = 0x8
    };
    Q_DECLARE_FLAGS(PluginTypes, PluginType)
    PluginContainer(QString fileName,QString pluginName,PluginType type);
    ~PluginContainer();
    SnorePlugin *load();
    const QString &file();
    const QString &name();
    PluginContainer::PluginType type();


    static PluginContainer::PluginType typeFromString(const QString &t);
    static const QStringList &types();

private:
    QPointer<SnorePlugin> m_instance;
    QString m_pluginFile;
    QString m_pluginName;
    PluginContainer::PluginType m_pluginType;
};
}

Q_DECLARE_OPERATORS_FOR_FLAGS(Snore::PluginContainer::PluginTypes)

#endif//PLUGINCONTAINER_H
