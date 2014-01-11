#ifndef SNORECOREPRIVATE_H
#define SNORECOREPRIVATE_H

#include "snore.h"

#include <QDir>

namespace Snore
{
class SNORE_EXPORT SnoreCorePrivate : public QObject
{
    Q_DECLARE_PUBLIC(SnoreCore)
    Q_OBJECT

public:
    static const QString snoreTMP();
    static const QDir &pluginDir();
public:
    SnoreCorePrivate(QSystemTrayIcon *trayIcon);
    ~SnoreCorePrivate();


    void notificationActionInvoked(Notification notification) const;

signals:
    void applicationRegistered(Snore::Application*);
    void applicationDeregistered(Snore::Application*);
    void notify(Snore::Notification noti);

private slots:
    void slotNotificationClosed(Snore::Notification);

private:
    SnoreCore *q_ptr;
    Hint m_hints;

    ApplicationsList m_applications;


    QStringList m_notificationBackends;
    QStringList m_Frontends;
    QStringList m_secondaryNotificationBackends;
    QStringList m_plugins;

    QPointer<SnoreBackend> m_notificationBackend;

    QSystemTrayIcon *m_trayIcon;
};
}

#endif // SNORECOREPRIVATE_H
