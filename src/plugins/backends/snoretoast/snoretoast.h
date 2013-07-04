#ifndef TOASTER_H
#define TOASTER_H

#include "core/plugins/snorebackend.h"

class SnoreToast : public Snore::SnoreBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreBackend)
public:
    SnoreToast();
    ~SnoreToast();
    bool init(Snore::SnoreCore *snore);


    // SnoreBackend interface
public slots:
    void slotRegisterApplication(Snore::Application *application);
    void slotUnregisterApplication(Snore::Application *application);
    uint slotNotify(Snore::Notification notification);
    bool slotCloseNotification(Snore::Notification notification);

private slots:
    void slotToastNotificationClosed(int code, QProcess::ExitStatus);

private:
    QString m_appID;


};

#endif // TOASTER_H
