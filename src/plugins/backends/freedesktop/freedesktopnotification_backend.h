#ifndef FREEDESKTOPNOTIFICATION_H
#define FREEDESKTOPNOTIFICATION_H
#include "libsnore/plugins/snorebackend.h"
#include "notificationinterface.h"

class  FreedesktopBackend: public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0" FILE "plugin.json")
public:
    FreedesktopBackend();
    virtual bool initialize() override;
    virtual bool deinitialize() override;

public slots:
    void slotNotify(Snore::Notification notification) override;
    void slotCloseNotification(Snore::Notification notification) override;

    void slotActionInvoked(const uint &id, const QString &actionID);
    void slotNotificationClosed(const uint &id, const uint &reason);

private:
    org::freedesktop::Notifications *m_interface;
    QHash<uint, Snore::Notification> m_dbusIdMap;

};

#endif // FREEDESKTOPNOTIFICATION_H
