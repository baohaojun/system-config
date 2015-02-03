
#include "core/settingsdialog.h"
#include "core/snore.h"
#include "core/version.h"

#include <QApplication>
#include <QMainWindow>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setApplicationName("SnoreSettings");
    app.setOrganizationName("SnoreNotify");
    app.setApplicationVersion(Snore::Version::version());
    app.setQuitOnLastWindowClosed(true);

    Snore::SnoreCore snore;
    snore.loadPlugins(Snore::SnorePlugin::ALL);

    Snore::SettingsDialog diag(&snore);
//    diag.setWindowIcon(QIcon(":/root/snore.png"));
    diag.setWindowTitle("SnoreSettings");
    diag.show();
    return app.exec();
}

