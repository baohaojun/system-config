/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2014-2015  Patrick von Reth <vonreth@kde.org>

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

#include <libsnore/snore.h>
#include <libsnore/notification/notification.h>
#include <libsnore/log.h>
#include <libsnore/version.h>
#include <libsnore/utils.h>

#include <QApplication>
#include <QCommandLineParser>

#include <iostream>

#ifdef Q_OS_WIN
#include <windows.h>
#include <windowsx.h>
#include <shellapi.h>
#include <winuser.h>
#endif

using namespace Snore;
using namespace std;

void bringToFront(QString pid)
{
    snoreDebug(SNORE_DEBUG) << pid;
#ifdef Q_OS_WIN
    auto findWindowForPid = [](ulong pid) {
        // based on http://stackoverflow.com/a/21767578
        pair<ulong, HWND> data = make_pair(pid, (HWND)0);
        ::EnumWindows((WNDENUMPROC)static_cast<BOOL(*)(HWND, LPARAM)>([](HWND handle, LPARAM lParam) -> BOOL {
            auto isMainWindow = [](HWND handle)
            {
                return ::GetWindow(handle, GW_OWNER) == (HWND)0 && IsWindowVisible(handle);
            };
            pair<ulong, HWND> &data = *(pair<ulong, HWND> *)lParam;
            ulong process_id = 0;
            ::GetWindowThreadProcessId(handle, &process_id);
            if (data.first != process_id || !isMainWindow(handle))
            {
                return TRUE;
            }
            data.second = handle;
            return FALSE;
        }), (LPARAM)&data);
        return data.second;
    };

    HWND wid = findWindowForPid(pid.toInt());
    if (wid) {
        Utils::bringWindowToFront((WId)wid, true);
    }
#endif
}

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setApplicationName(QLatin1String("snoresend"));
    app.setOrganizationName(QLatin1String("Snorenotify"));
    app.setApplicationVersion(Snore::Version::version());

    QCommandLineParser parser;
    parser.setApplicationDescription(QLatin1String("A command line interface for Snorenotify."));
    parser.addHelpOption();
    parser.addVersionOption();

    QCommandLineOption title(QStringList() << QLatin1String("t") << QLatin1String("title"), QLatin1String("Set the notification title."), QLatin1String("title"));
    parser.addOption(title);

    QCommandLineOption message(QStringList() << QLatin1String("m") << QLatin1String("message"), QLatin1String("Set the notification body."), QLatin1String("message"));
    parser.addOption(message);

    QCommandLineOption applicationName(QStringList() << QLatin1String("a") << QLatin1String("application"), QLatin1String("Set the notification applicattion."), QLatin1String("application"), app.applicationName());
    parser.addOption(applicationName);

    QCommandLineOption alertName(QStringList() << QLatin1String("c") << QLatin1String("alert"), QLatin1String("Set the notification alert class."), QLatin1String("alert"), QLatin1String("Default"));
    parser.addOption(alertName);

    QCommandLineOption iconPath(QStringList() << QLatin1String("i") << QLatin1String("icon"), QLatin1String("Set the notification icon."), QLatin1String("icon"));
    parser.addOption(iconPath);

    QCommandLineOption priority(QStringList() << QLatin1String("p") << QLatin1String("priority"), QLatin1String("Set the notification's priority."), QLatin1String("[-2, 2]"), QLatin1String("0"));
    parser.addOption(priority);

    QCommandLineOption markup(QStringList() << QLatin1String("markup"), QLatin1String("Enable markup support."), QLatin1String("[0,1]"), QLatin1String("0"));
    parser.addOption(markup);

    QCommandLineOption silent(QStringList() << QLatin1String("silent"), QLatin1String("Don't print to stdout."));
    parser.addOption(silent);

    QCommandLineOption _bringProcessToFront(QStringList() << QLatin1String("bring-process-to-front"), QLatin1String("Bring process with pid to front if notification is clicked."), QLatin1String("pid"));
    parser.addOption(_bringProcessToFront);

    QCommandLineOption _bringWindowToFront(QStringList() << QLatin1String("bring-window-to-front"), QLatin1String("Bring window with wid to front if notification is clicked."), QLatin1String("wid"));
    parser.addOption(_bringWindowToFront);

    parser.process(app);
    snoreDebug(SNORE_DEBUG) << app.arguments();
    if (parser.isSet(title) && parser.isSet(message)) {
        SnoreCore &core = SnoreCore::instance();

        core.loadPlugins(SnorePlugin::BACKEND | SnorePlugin::SECONDARY_BACKEND);

        Icon icon = Icon::defaultIcon();
        if (parser.isSet(iconPath)) {
            icon = Icon(parser.value(iconPath));
        }
        Application application(parser.value(applicationName), icon);
        Alert alert(parser.value(alertName), icon);
        application.addAlert(alert);

        if (parser.value(markup).toInt() == 1) {
            application.hints().setValue("use-markup", true);
        }

        core.registerApplication(application);

        int prio = parser.value(priority).toInt();
        if (prio < -2 || prio > 2) {
            parser.showHelp(-1);
        }
        Notification n(application, alert, parser.value(title), parser.value(message), icon, Notification::defaultTimeout(), static_cast<Notification::Prioritys>(prio));
        if (parser.isSet(_bringProcessToFront) || parser.isSet(_bringWindowToFront)) {
            n.addAction(Action(1, qApp->translate("SnoreSend", "Bring to Front")));
        }
        int returnCode = -1;

        app.connect(&core, &SnoreCore::notificationClosed, [&](Notification noti) {
            if (!parser.isSet(silent)) {
                QString reason;
                QDebug(&reason) << noti.closeReason();
                cout << qPrintable(reason) << endl;
            }
            if (noti.closeReason() == Notification::CLOSED) {
                if (parser.isSet(_bringProcessToFront)) {
                    bringToFront(parser.value(_bringProcessToFront));
                } else if (parser.isSet(_bringWindowToFront)) {
                    Utils::bringWindowToFront((WId)parser.value(_bringWindowToFront).toULongLong(), true);
                }
            }
            returnCode = noti.closeReason();
        });
        app.connect(&core, &SnoreCore::notificationClosed, &app, &QApplication::quit);
        app.processEvents();
        core.broadcastNotification(n);

        app.exec();
        return returnCode;
    }
    parser.showHelp(1);
}

