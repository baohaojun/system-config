#ifndef SNORE_STATIC_PLUGINS_H
#define SNORE_STATIC_PLUGINS_H

#include <QCoreApplication>
#include <QtPlugin>

#define SNORE_STATIC 0
#define SNORE_STATIC_QT 0
#define Qt5Quick_FOUND 0


#ifndef SNORE_CONFIG_ONLY
#if SNORE_STATIC_QT

#if Qt5Quick_FOUND
    Q_IMPORT_PLUGIN(QtQuick2Plugin)
    Q_IMPORT_PLUGIN(QtQuick2WindowPlugin)
#endif

#ifdef Q_OS_WIN
    Q_IMPORT_PLUGIN(QWindowsIntegrationPlugin)
#else
#error Unsupported platform
#endif
#endif

#if SNORE_STATIC
namespace SnorePlugin {}


using namespace SnorePlugin;


namespace {
    static void loadSnoreResources()
    {
        // prevent multiple symbols
         static const auto load = []() {
             Q_INIT_RESOURCE(snore);
         };
         load();
    }
}
Q_COREAPP_STARTUP_FUNCTION(loadSnoreResources)
#endif

#endif
#endif
