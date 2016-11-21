#ifndef TOASTER_H
#define TOASTER_H

#include "libsnore/plugins/snorebackend.h"
#include <QProcess>

namespace SnorePlugin {

class WindowsToast : public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.NotificationBackend/1.0" FILE "snore_plugin.json")
public:
    WindowsToast() = default;
    ~WindowsToast() = default;

    virtual bool canCloseNotification() const override;

    bool isReady() override;
public Q_SLOTS:
    void slotNotify(Snore::Notification notification) override;
    void slotRegisterApplication(const Snore::Application &application) override;
    void slotCloseNotification(Snore::Notification notification) override;

private:
    QString appId(const Snore::Application &application);

    QProcess *createProcess(Snore::Notification noti);

};
}

#endif // TOASTER_H
