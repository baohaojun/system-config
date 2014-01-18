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

#ifndef SNORE_PLUGINS_H
#define SNORE_PLUGINS_H
#include "../snore_exports.h"
#include "../notification/notification.h"

#include <QPointer>
#include <QHash>
#include <QTimer>
#include <QtPlugin>


namespace Snore{
class Application;
class SnoreCore;


class SNORE_EXPORT SnorePlugin : public QObject
{
    Q_OBJECT
public:
    enum PluginType{
        ALL = 0x0,//for loading plugins
        BACKEND = 0x1,
        SECONDARY_BACKEND = 0x2,
        FRONTEND = 0x4,
        PLUGIN = 0x8
    };
    Q_DECLARE_FLAGS(PluginTypes, PluginType)

    SnorePlugin ( const QString &name);
    virtual ~SnorePlugin();
    virtual bool initialize( SnoreCore *snore );
    virtual bool deinitialize();
    bool isInitialized();
    SnoreCore* snore();
    const QString &name() const;

private:
    SnorePlugin() {}
    QString m_name;
    bool m_initialized;
    QPointer<SnoreCore> m_snore;


};
Q_DECLARE_OPERATORS_FOR_FLAGS(Snore::SnorePlugin::PluginTypes)


}
Q_DECLARE_INTERFACE ( Snore::SnorePlugin,
                      "org.Snore.SnorePlugin/1.0" )

//compatability defines to reduce the number of ifdefs to make fiat compile with qt4 and qt5
#if QT_VERSION >= QT_VERSION_CHECK(5,0,0)
#   if defined(Q_EXPORT_PLUGIN)
#       undef Q_EXPORT_PLUGIN
#   endif
#   if defined(Q_EXPORT_PLUGIN2)
#       undef Q_EXPORT_PLUGIN2
#   endif
#   define Q_EXPORT_PLUGIN(a)
#   define Q_EXPORT_PLUGIN2(a, b)
#else
#   define Q_PLUGIN_METADATA(a)
#endif



#endif//SNORE_PLUGINS_H
