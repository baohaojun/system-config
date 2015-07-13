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

    virtual bool canCloseNotification() const override;

public Q_SLOTS:
    virtual void slotInitialize() override;
    void slotNotify(Snore::Notification notification) override;
    void slotRegisterApplication(const Snore::Application &application) override;
    void slotCloseNotification(Snore::Notification notification) override;

private:
    QString appId(const Snore::Application &application);

    QProcess *createProcess(Snore::Notification noti);

};

#endif // TOASTER_H
