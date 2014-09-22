#ifndef TOASTY_H
#define TOASTY_H

#include "core/plugins/snorebackend.h"

#include <QNetworkAccessManager>

class Toasty : public Snore::SnoreSecondaryBackend
{
    Q_OBJECT
    Q_INTERFACES(Snore::SnoreSecondaryBackend)
    Q_PLUGIN_METADATA(IID "org.Snore.SecondaryNotificationBackend/1.0" FILE "plugin.json")
public:
    Toasty();
    ~Toasty();
    virtual bool initialize(Snore::SnoreCore *snore) override;
    virtual bool deinitialize() override;

public slots:
    virtual void slotNotify(Snore::Notification notification) override;

private:
    QString m_key;
    QNetworkAccessManager m_manager;

};

#endif // TOASTY_H
