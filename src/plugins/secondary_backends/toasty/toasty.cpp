#include "toasty.h"

#include <QtNetwork>

Q_EXPORT_PLUGIN2(libsnore_secondary_backend_toasty, Toasty)

using namespace Snore;

Toasty::Toasty():
    SnoreSecondaryBackend("Toasty", false)
{
}

void Toasty::slotNotify(Notification notification)
{
    QString id = value("DEVICEID").toString();
    if (!id.isEmpty()) {
        QNetworkRequest request(QString("http://api.supertoasty.com/notify/%1").arg(id));
        QHttpMultiPart *mp = new QHttpMultiPart(QHttpMultiPart::FormDataType);
        QHttpPart title;
        title.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant("form-data; name=\"title\""));
        title.setBody(Snore::toPlainText(notification.title()).toLatin1().constData());
        mp->append(title);

        QHttpPart text;
        text.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant("form-data; name=\"text\""));
        text.setBody(Snore::toPlainText(notification.text()).toLatin1().constData());
        mp->append(text);

        QHttpPart app;
        app.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant("form-data; name=\"sender\""));
        app.setBody(notification.application().name().toLatin1().constData());
        mp->append(app);

        QHttpPart icon;
        Icon sIcon(notification.icon().scaled(QSize(127, 127)));
        icon.setHeader(QNetworkRequest::ContentDispositionHeader, QVariant(QString("form-data; name=\"image\"; filename=\"%1\"").arg(sIcon.localUrl())));
        icon.setHeader(QNetworkRequest::ContentTypeHeader, QVariant("image/png"));
        QFile *file = new QFile(sIcon.localUrl());
        file->open(QIODevice::ReadOnly);
        icon.setBodyDevice(file);
        mp->append(icon);

        QNetworkReply *reply =  m_manager.post(request, mp);
        mp->setParent(reply);
        file->setParent(reply);

        connect(reply, SIGNAL(finished()), this, SLOT(slotRequestFinished()));

    }
}

void Toasty::slotRequestFinished()
{
    QNetworkReply *reply = qobject_cast<QNetworkReply *>(sender());
    snoreDebug(SNORE_DEBUG) << reply->readAll();
    reply->close();
    reply->deleteLater();
}

bool Toasty::initialize(SnoreCore *snore)
{
    if (SnoreSecondaryBackend::initialize(snore)) {
        setDefaultValue("DEVICEID", "");
        snoreDebug(SNORE_DEBUG) << value("DEVICEID");
        return true;
    }
    return false;
}

bool Toasty::deinitialize()
{
    return SnoreSecondaryBackend::deinitialize();
}
