
#include "snorenotify.h"
#include "libsnore/log.h"
#include "libsnore/version.h"

#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setApplicationName("SnoreNotify");
    app.setOrganizationName("SnoreNotify");
    app.setApplicationVersion(Snore::Version::version());
    app.setQuitOnLastWindowClosed(false);

    SnoreNotify sn;
    return app.exec();
}

