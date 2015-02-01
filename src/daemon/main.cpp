
#include "snorenotify.h"
#include "core/log.h"
#include "core/version.h"

#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setApplicationName("SnoreNotify");
    app.setOrganizationName("SnoreNotify");
    app.setApplicationVersion(Snore::Version::version());

    SnoreNotify sn;
    return app.exec();
}

