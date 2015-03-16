
#include "libsnore/snore.h"
#include "libsnore/version.h"
#include "settingswindow.h"

#include <QApplication>
#include <QMainWindow>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setApplicationName("SnoreSettings");
    app.setOrganizationName("SnoreNotify");
    app.setApplicationVersion(Snore::Version::version());

    Snore::SnoreCore::instance().loadPlugins(Snore::SnorePlugin::ALL);
    SettingsWindow window;
    window.show();
    return app.exec();
}

