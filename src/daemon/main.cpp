
#include "snorenotify.h"
#include "libsnore/log.h"
#include "libsnore/version.h"

#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setApplicationName(QLatin1String("SnoreNotify"));
    app.setOrganizationName(QLatin1String("SnoreNotify"));
    app.setApplicationVersion(Snore::Version::version());
    app.setQuitOnLastWindowClosed(false);

    SnoreNotify sn;
    return app.exec();
}

