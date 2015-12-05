/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2014-2015  Hannah von Reth <vonreth@kde.org>

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
#include <libsnore/version.h>
#include <libsnore/utils.h>

#include <QGuiApplication>
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
    qCDebug(SNORE) << pid;
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
        Utils::bringWindowToFront(wid, true);
    }
#endif
}

int main(int argc, char *argv[])
{
    QGuiApplication app(argc, argv);
    app.setApplicationName(QStringLiteral("snoresend"));
    app.setOrganizationName(QStringLiteral("Snorenotify"));
    app.setApplicationVersion(Snore::Version::version());

    QCommandLineParser parser;
    parser.setApplicationDescription(QStringLiteral("A command line interface for Snorenotify."));
    parser.addHelpOption();
    parser.addVersionOption();

    QCommandLineOption title(QStringList() << QStringLiteral("t") << QStringLiteral("title"), QStringLiteral("Set the notification title."), QStringLiteral("title"));
    parser.addOption(title);

    QCommandLineOption message(QStringList() << QStringLiteral("m") << QStringLiteral("message"), QStringLiteral("Set the notification body."), QStringLiteral("message"));
    parser.addOption(message);

    QCommandLineOption applicationName(QStringList() << QStringLiteral("a") << QStringLiteral("application"), QStringLiteral("Set the notification applicattion."), QStringLiteral("application"), app.applicationName());
    parser.addOption(applicationName);

    QCommandLineOption alertName(QStringList() << QStringLiteral("c") << QStringLiteral("alert"), QStringLiteral("Set the notification alert class."), QStringLiteral("alert"), QStringLiteral("Default"));
    parser.addOption(alertName);

    QCommandLineOption iconPath(QStringList() << QStringLiteral("i") << QStringLiteral("icon"), QStringLiteral("Set the notification icon."), QStringLiteral("icon"));
    parser.addOption(iconPath);

    QCommandLineOption priority(QStringList() << QStringLiteral("p") << QStringLiteral("priority"), QStringLiteral("Set the notification's priority."), QStringLiteral("[-2, 2]"), QStringLiteral("0"));
    parser.addOption(priority);

    QCommandLineOption markup(QStringList() << QStringLiteral("markup"), QStringLiteral("Enable markup support."), QStringLiteral("[0,1]"), QStringLiteral("0"));
    parser.addOption(markup);

    QCommandLineOption silent(QStringList() << QStringLiteral("silent"), QStringLiteral("Don't print to stdout."));
    parser.addOption(silent);

    QCommandLineOption _bringProcessToFront(QStringList() << QStringLiteral("bring-process-to-front"), QStringLiteral("Bring process with pid to front if notification is clicked."), QStringLiteral("pid"));
    parser.addOption(_bringProcessToFront);

    parser.process(app);
    qCDebug(SNORE) << app.arguments();
    if (parser.isSet(title) && parser.isSet(message)) {
        SnoreCore &core = SnoreCore::instance();

        core.loadPlugins(SnorePlugin::Backend | SnorePlugin::SecondaryBackend);

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
        if (parser.isSet(_bringProcessToFront)) {
            n.addAction(Action(1, qApp->translate("SnoreSend", "Bring to Front")));
        }
        int returnCode = -1;

        app.connect(&core, &SnoreCore::notificationClosed, [&](Notification noti) {
            if (!parser.isSet(silent)) {
                QString reason;
                QDebug(&reason) << noti.closeReason();
                cout << qPrintable(reason) << endl;
            }
            if (noti.closeReason() == Notification::Closed) {
                if (parser.isSet(_bringProcessToFront)) {
                    bringToFront(parser.value(_bringProcessToFront));
                }
            }
            returnCode = noti.closeReason();
        });
        app.connect(&core, &SnoreCore::notificationClosed, &app, &QGuiApplication::quit);
        app.processEvents();
        core.broadcastNotification(n);

        app.exec();
        return returnCode;
    }
    parser.showHelp(1);
}

