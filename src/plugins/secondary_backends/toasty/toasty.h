#ifndef TOASTY_H
#define TOASTY_H

#include "core/plugins/snorebackend.h"

#include <QNetworkAccessManager>

class Toasty : public Snore::SnoreSecondaryBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreSecondaryBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.SecondaryNotificationBackend/1.0")
public:
    Toasty();
    virtual bool initialize(Snore::SnoreCore *snore);
    virtual bool deinitialize();

public slots:
    virtual void slotNotify(Snore::Notification notification);

private slots:
    void slotRequestFinished();

private:
    QNetworkAccessManager m_manager;

};

#endif // TOASTY_H
