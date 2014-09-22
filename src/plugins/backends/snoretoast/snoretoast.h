#ifndef TOASTER_H
#define TOASTER_H

#include "core/plugins/snorebackend.h"
#include <QProcess>

class SnoreToast : public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0" FILE "plugin.json")
public:
    SnoreToast();
    ~SnoreToast();
    virtual bool initialize(Snore::SnoreCore *snore);

public slots:
    void slotNotify(Snore::Notification notification);
    void slotRegisterApplication(const Snore::Application &application);
    void slotCloseNotification(Snore::Notification notification);

private slots:
    void slotToastNotificationClosed(int code, QProcess::ExitStatus);

private:
    QString appId(const Snore::Application &application);

};

#endif // TOASTER_H
