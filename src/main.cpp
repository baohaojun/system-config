
#include <libsnore/snore.h>
#include <libsnore/notification/notification.h>
#include <libsnore/log.h>
#include <libsnore/version.h>

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

void bringToFront(QString pid, Notification noti)
{
    Q_UNUSED(pid)
    Q_UNUSED(noti)
#ifdef Q_OS_WIN
    auto findWindowForPid = [](ulong pid)
    {
        // based on http://stackoverflow.com/a/21767578
        pair<ulong, HWND> data = make_pair(pid, (HWND)0);
        ::EnumWindows([](HWND handle, LPARAM lParam) -> BOOL{
            auto isMainWindow = [](HWND handle){
                return ::GetWindow(handle, GW_OWNER) == (HWND)0 && IsWindowVisible(handle);
            };
            pair<ulong, HWND>& data = *(pair<ulong, HWND>*)lParam;
            ulong process_id = 0;
            ::GetWindowThreadProcessId(handle, &process_id);
            if (data.first != process_id || !isMainWindow(handle)) {
                return TRUE;
            }
            data.second = handle;
            return FALSE;
        }, (LPARAM)&data);
        return data.second;
    };

    HWND wid = findWindowForPid(pid.toInt());
    if(wid) {
        HWND hwndActiveWin = GetForegroundWindow();
        int idActive = GetWindowThreadProcessId( hwndActiveWin, NULL );

        if ( AttachThreadInput(GetCurrentThreadId(), idActive, TRUE) )
        {
            SetForegroundWindow( wid );
            SetFocus( wid );
            FlashWindow( wid, TRUE );
            AttachThreadInput( GetCurrentThreadId(), idActive, FALSE );
        } else {
            // try it anyhow
            SetForegroundWindow( wid );
            SetFocus( wid );
            FlashWindow( wid, TRUE );
        }
    }
#endif
}

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    app.setApplicationName("snore-send");
    app.setOrganizationName("Snorenotify");
    app.setApplicationVersion(Snore::Version::version());

    QCommandLineParser parser;
    parser.setApplicationDescription("A command line interface for Snorenotify.");
    parser.addHelpOption();
    parser.addVersionOption();

    QCommandLineOption title(QStringList() << "t" << "title", "Set the notification title.", "title");
    parser.addOption(title);

    QCommandLineOption message(QStringList() << "m" << "message", "Set the notification body.", "message");
    parser.addOption(message);

    QCommandLineOption applicationName(QStringList() << "a" << "application", "Set the notification applicattion.", "application", app.applicationName());
    parser.addOption(applicationName);

    QCommandLineOption alertName(QStringList() << "c" << "alert", "Set the notification alert class.", "alert", "Default");
    parser.addOption(alertName);

    QCommandLineOption iconPath(QStringList() << "i" << "icon", "Set the notification icon.", "icon", ":/root/snore.png");
    parser.addOption(iconPath);

    QCommandLineOption silent(QStringList() << "silent", "Don't print to stdout.");
    parser.addOption(silent);

    QCommandLineOption _bringToFront(QStringList() << "bring-to-front", "Bring process with pid to front if notification is clicked.", "pid");
    parser.addOption(_bringToFront);



    parser.process(app);
    if (parser.isSet(title) && parser.isSet(message)) {
        SnoreCore &core = SnoreCore::instance();

        core.loadPlugins(SnorePlugin::BACKEND | SnorePlugin::SECONDARY_BACKEND);

        Icon icon(parser.value(iconPath));
        Application application(parser.value(applicationName), icon);
        Alert alert(parser.value(alertName), icon);
        application.addAlert(alert);

        core.registerApplication(application);

        Notification n(application, alert, parser.value(title), parser.value(message), icon);

        core.broadcastNotification(n);
        int returnCode = -1;

        app.connect(&core, &SnoreCore::notificationClosed, [&](Notification noti) {
            if (!parser.isSet(silent)) {
                QString reason;
                QDebug(&reason) << noti.closeReason();
                cout << qPrintable(reason) << endl;
            }
            if(noti.closeReason() == Notification::CLOSED  && parser.isSet(_bringToFront)) {
                bringToFront(parser.value(_bringToFront), noti);
            }
            returnCode = noti.closeReason();
        });
        app.connect(&core, &SnoreCore::notificationClosed, &app, &QApplication::quit);
        app.exec();
        return returnCode;
    }
    parser.showHelp(1);
}

