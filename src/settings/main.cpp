/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2015  Patrick von Reth <vonreth@kde.org>

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

#include <QApplication>
#include <QCommandLineParser>

#include <iostream>

using namespace Snore;
using namespace std;

bool setSetting(const QString &appName, SettingsType type, const QString &_key, const QString &value)
{
    QSettings &settings = SettingsWindow::settings();
    QString key = Utils::normalizeSettingsKey(_key, type, appName);
    QVariant oldValue = settings.value(key);

    //TODO: make sure that the values are valid qvariant.canConvert doesnt work.
    if (!oldValue.isValid()) {
        cout << "Invalid key: " << qPrintable(key) << endl;
        return false;
    }
    settings.setValue(key, value);
    cout << "Set: " << qPrintable(key) << " to " << qPrintable(settings.value(key).toString()) << endl;
    return true;
}

void listApps()
{
    for (const QString &app : SettingsWindow::knownApps()) {
        cout << qPrintable(app) << endl;
    }
}

void listSettings(SettingsType type, const QString &application)
{
    QSettings &settings = SettingsWindow::settings();
    auto getAllKeys = [](QSettings & settings) {
        return settings.allKeys();
    };

    QString prefix = application;
    if(application == QLatin1String("global")) {
        prefix = QString();
    }
    cout << qPrintable(application) << endl;
    for (const QString &key : SettingsWindow::allSettingsKeysWithPrefix(
                Utils::normalizeSettingsKey(QLatin1String(""), type, prefix), settings, getAllKeys)) {
        cout << "  " << qPrintable(key) << ": " << qPrintable(settings.value(Utils::normalizeSettingsKey(key, type, prefix)).toString()) << endl;
    }
}

int main(int argc, char *argv[])
{

    SettingsWindow *window;

    QApplication app(argc, argv);
    app.setApplicationName(QLatin1String("SnoreSettings"));
    app.setOrganizationName(QLatin1String("SnoreNotify"));
    app.setApplicationVersion(Snore::Version::version());


    Snore::SnoreCore::instance().loadPlugins(Snore::SnorePlugin::ALL);
    Snore::SnoreCorePrivate::instance()->defaultApplication().hints().setValue("use-markup", QVariant::fromValue(true));

    QCommandLineParser parser;
    parser.setApplicationDescription(QLatin1String("A settings interface for Snorenotify."));
    parser.addHelpOption();
    parser.addVersionOption();

    //TODO: merge with appNameCommand ?
    QCommandLineOption listAppsCommand(QLatin1String("apps"), QLatin1String("List possible application."));
    parser.addOption(listAppsCommand);

    QCommandLineOption listSettingsCommand({QLatin1String("l"), QLatin1String("list")} , QLatin1String("List settings for the given --appName or the global settings."));
    parser.addOption(listSettingsCommand);

    QCommandLineOption appNameCommand({QLatin1String("a"), QLatin1String("appName")} , QLatin1String("Set the Name of the app <app> or global."), QLatin1String("app"), QLatin1String("global"));
    parser.addOption(appNameCommand);

    parser.addPositionalArgument(QLatin1String("key"), QLatin1String("The settings Key."));
    parser.addPositionalArgument(QLatin1String("value"), QLatin1String("The new settings Value"));

    parser.process(app);


    QString appName = parser.value(appNameCommand);

    SettingsType type = GLOBAL_SETTING;
    if(appName != QLatin1String("global")){
        type = LOCAL_SETTING;
    }

    if (parser.isSet(listAppsCommand)) {
        listApps();
    } else if (parser.isSet(listSettingsCommand)) {
        listSettings(type, appName);
    } else if (parser.optionNames().empty() && parser.positionalArguments().empty()) {
        window = new SettingsWindow();
        window->show();
        return app.exec();
    } else {
        QStringList posArgs = parser.positionalArguments();
        if (posArgs.size() != 2) {
            parser.showHelp(1);
        }
        if (!setSetting(appName, type, posArgs[0], posArgs[1])) {
            return 1;
        }
    }
    app.processEvents();
    return 0;
}

