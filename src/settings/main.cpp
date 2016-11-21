/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2015  Hannah von Reth <vonreth@kde.org>

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
#include "libsnore/snore.h"
#include "libsnore/snore_p.h"
#include "libsnore/version.h"
#include "settingswindow.h"

#include "libsnore/snore_static_plugins.h"

#include <QApplication>
#include <QCommandLineParser>

#include <iostream>

using namespace Snore;
using namespace std;

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setApplicationName(QStringLiteral("SnoreSettings"));
    app.setOrganizationName(QStringLiteral("SnoreNotify"));
    app.setApplicationVersion(Snore::Version::version());

    Snore::SnoreCore::instance().loadPlugins(Snore::SnorePlugin::All);
    Snore::SnoreCorePrivate::instance()->defaultApplication().hints().setValue("use-markup", QVariant::fromValue(true));

    QCommandLineParser parser;
    parser.setApplicationDescription(QStringLiteral("A settings interface for Snorenotify."));
    parser.addHelpOption();
    parser.addVersionOption();

    QCommandLineOption appNameCommand({QStringLiteral("a"), QStringLiteral("appName") } , QStringLiteral("Set the Name of the app <app> or global."), QStringLiteral("app"), QStringLiteral("global"));
    parser.addOption(appNameCommand);

    parser.process(app);

    QString appName = parser.value(appNameCommand);

    SettingsWindow *window = new SettingsWindow(appName);
    window->show();
    return qApp->exec();
}
