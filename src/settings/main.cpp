
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
    for (const QString &key : SettingsWindow::allSettingsKeysWithPrefix(
                Utils::normalizeSettingsKey(QLatin1String(""), type, application), settings, getAllKeys)) {
        cout << "  " << qPrintable(key) << ": " << qPrintable(settings.value(Utils::normalizeSettingsKey(key, type, application)).toString()) << endl;
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

    QCommandLineOption appNameCommand({QLatin1String("a"), QLatin1String("appName")} , QLatin1String("Set the Name of the app <app>."), QLatin1String("app"), QLatin1String("GlobalSettings"));
    parser.addOption(appNameCommand);

    QCommandLineOption typeCommand({QLatin1String("t"), QLatin1String("type")} , QLatin1String("Type of the setting [global|local]."), QLatin1String("type"));
    parser.addOption(typeCommand);

    parser.addPositionalArgument(QLatin1String("key"), QLatin1String("The settings Key."));
    parser.addPositionalArgument(QLatin1String("value"), QLatin1String("The new settings Value"));

    parser.process(app);

    SettingsType type = GLOBAL_SETTING;
    if (parser.isSet(typeCommand)) {
        QString typeName = parser.value(typeCommand);
        if (typeName != QLatin1String("global") && typeName == QLatin1String("local")) {
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
        window = new SettingsWindow();
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

