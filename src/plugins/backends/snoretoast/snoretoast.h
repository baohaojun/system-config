#ifndef TOASTER_H
#define TOASTER_H

#include "libsnore/plugins/snorebackend.h"
#include <QProcess>

class SnoreToast : public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0" FILE "plugin.json")
public:
    SnoreToast() = default;
    ~SnoreToast() = default;
    virtual bool initialize() override;

    virtual bool canCloseNotification() const override;

public slots:
    void slotNotify(Snore::Notification notification) override;
    void slotRegisterApplication(const Snore::Application &application) override;
    void slotCloseNotification(Snore::Notification notification) override;

private slots:
    void slotToastNotificationClosed(int code, QProcess::ExitStatus);
    void slotPrintExitStatus(int code, QProcess::ExitStatus);
private:
    QString appId(const Snore::Application &application);

};

#endif // TOASTER_H
