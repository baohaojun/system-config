
#include "libsnore/snore.h"
#include "libsnore/snore_p.h"
#include "libsnore/version.h"
#include "settingswindow.h"

#include <QApplication>
#include <QCommandLineParser>
#include <QMainWindow>

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

    cout << qPrintable(application) << endl;
    for (const QString &key : Utils::allSettingsKeysWithPrefix(
                Utils::normalizeSettingsKey("", type, application), settings, getAllKeys)) {
        cout << "  " << qPrintable(key) << ": " << qPrintable(settings.value(Utils::normalizeSettingsKey(key, type, application)).toString()) << endl;
    }
}

int main(int argc, char *argv[])
{

    QScopedPointer<SettingsWindow> window;

    QApplication app(argc, argv);
    app.setApplicationName("SnoreSettings");
    app.setOrganizationName("SnoreNotify");
    app.setApplicationVersion(Snore::Version::version());

    Snore::SnoreCore::instance().loadPlugins(Snore::SnorePlugin::ALL);

    QCommandLineParser parser;
    parser.setApplicationDescription("A settings interface for Snorenotify.");
    parser.addHelpOption();
    parser.addVersionOption();

    //TODO: merge with appNameCommand ?
    QCommandLineOption listAppsCommand("apps", "List possible application.");
    parser.addOption(listAppsCommand);

    QCommandLineOption listSettingsCommand({"l", "list"} , "List settings for the given --appName or the global settings.");
    parser.addOption(listSettingsCommand);

    QCommandLineOption appNameCommand({"a", "appName"} , "Set the Name of the app <app>.", "app", "GlobalSettings");
    parser.addOption(appNameCommand);

    QCommandLineOption typeCommand({"t", "type"} , "Type of the setting [global|local].", "type");
    parser.addOption(typeCommand);

    parser.addPositionalArgument("key", "The settings Key.");
    parser.addPositionalArgument("value", "The new settings Value");

    parser.process(app);

    SettingsType type = GLOBAL_SETTING;
    if (parser.isSet(typeCommand)) {
        QString typeName = parser.value(typeCommand);
        if (typeName != "global" && typeName == "local") {
            type = LOCAL_SETTING;
        } else {
            parser.showHelp(1);
        }
    }
    if (parser.isSet(appNameCommand)) {
        type = LOCAL_SETTING;
    }

    if (parser.isSet(listAppsCommand)) {
        listApps();
    } else if (parser.isSet(listSettingsCommand)) {
        listSettings(type, parser.value(appNameCommand));
    } else if (parser.optionNames().empty() && parser.positionalArguments().empty()) {
        window.reset(new SettingsWindow());
        window->show();
        return app.exec();
    } else {
        QStringList posArgs = parser.positionalArguments();
        if (posArgs.size() != 2) {
            parser.showHelp(1);
        }
        QString appName;
        if (parser.isSet(appNameCommand)) {
            appName = parser.value(appNameCommand);
        }
        if (!setSetting(appName, type, posArgs[0], posArgs[1])) {
            return 1;
        }
    }
    app.processEvents();
    return 0;
}

