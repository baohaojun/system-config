/*
    SnoreNotify is a Notification Framework based on Qt
    Copyright (C) 2013-2014  Patrick von Reth <vonreth@kde.org>

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

#ifndef APPLICATION_H
#define APPLICATION_H
#include "snore_exports.h"
#include "hint.h"
#include "notification/icon.h"
#include "alert.h"

#include <QHash>
namespace Snore
{

class ApplicationData;

/**
 *  Application contains all relevant data to manage applications with the notification backend.
 *  Application uses a shared datamodel, its content is never copied and automatically released.
 *
 * @author Patrick von Reth \<vonreth at kde.org\>
 */

class SNORE_EXPORT Application
{
public:
    Application();

    /**
     * Creates a new Application object
     * @param name
     * @param icon
     * @see SnoreCore::registerApplication
     */
    explicit Application(const QString &name, const Icon &icon);

    /**
     * The copy constructor
     * @param other
     */
    Application(const Application &other);

    /**
     * The copy operator
     * @param other
     */
    Application &operator=(const Application &other);

    ~Application();

    /**
     * Add an alert to the Application
     * @param alert the Alert
     */
    void addAlert(const  Alert &alert);

    /**
     *
     * @return the name of the Application
     */
    QString name() const;

    /**
     *
     * @return the default icon for Notifications ans Alerts of this Application.
     */
    const Icon &icon() const;

    /**
     *
     * @return a QHash with the Alers registered with this Application.
     */
    const QHash<QString, Alert> &alerts() const;

    /**
     *
     * @return the default alert for notifications.
     */
    const Alert defaultAlert() const;

    /**
     *
     * @return whether the Application is valid.
     */
    bool isValid() const;

    /**
     * Returns application specific hints.
     * Hints are inherited by Notification's.
     * Key              |   Type                    |    Value       |   Required
     * -------------    |   -----------             |   -----------  |   -----------
     * use-markup       |   QString                 |Enable markup support for title and message, strings must be html escaped. |  Many Backends.
     * desktop-entry    |   QString                 | The name of the desktop enty associated with the application.             |  Used for The freedesktop backend.
     * windows-app-id   |   QString                 | The app id associated with the application.                               |  Needed for the Windows 8 backend [See MSDN Documentation](http://msdn.microsoft.com/en-us/library/windows/apps/dd378459.aspx).
     * tray-icon        |   QPointer<QSystemTray>   | A QSystemTray item.                                                       |  Needed for the System Tray Backend.
     * pushover-token   |   QString                 | The token associated with your application.                               |  Needed to associate pushover notification with your application, to register your application visit [Pushover](https://pushover.net/apps/build).
     * silent           |   bool                    | Don't play notification sounds.                                           |  Multiple backends.
     * sound            |   QString                 | Local uri to a sound file.                                                | Secondary Backend Sound.
     */
    Hint &hints();

    /**
     * Same as hints
     * @see hints
     */
    const Hint &constHints() const;

private:
    QExplicitlySharedDataPointer<ApplicationData> d;

};

}
Q_DECLARE_METATYPE(Snore::Application)

SNORE_EXPORT QDebug operator<< (QDebug debug, const Snore::Application &app);

#endif // APPLICATION_H
