#include "snoretoast.h"
#include "libsnore/snore.h"
#include "libsnore/snore_p.h"
#include "libsnore/utils.h"

#include "libsnore/plugins/plugins.h"
#include "libsnore/plugins/snorebackend.h"

#include <QDir>
#include <QApplication>
#include <QSysInfo>

#include <windows.h>

using namespace Snore;

bool SnoreToast::isReady()
{
    if (errorString().isEmpty() && QSysInfo::windowsVersion() < QSysInfo::WV_WINDOWS8) {
        setErrorString(tr("%1 needs at least Windows 8 to run.").arg(name()));
        return false;
    }
    return true;
}

bool SnoreToast::canCloseNotification() const
{
    return true;
}

void SnoreToast::slotNotify(Notification notification)
{
    QProcess *p = createProcess(notification);

    QStringList arguements;
    arguements << QLatin1String("-t")
               << notification.title()
               << QLatin1String("-m")
               << notification.text()
               << QLatin1String("-p")
               << QDir::toNativeSeparators(notification.icon().localUrl(QSize(1024, 1024)))
               << QLatin1String("-w")
               << QLatin1String("-appID")
               << appId(notification.application())
               << QLatin1String("-id")
               << QString::number(notification.id());
    //TODO: could clash with sound backend
    if (notification.hints().value("silent").toBool() || notification.hints().value("sound").isValid()) {
        arguements << QLatin1String("-silent");
    }
    snoreDebug(SNORE_DEBUG) << "SnoreToast" << arguements;

    connect(p, static_cast<void (QProcess::*)(int, QProcess::ExitStatus)>(&QProcess::finished), [this, notification](int code) {
        Notification::CloseReasons reason = Notification::NONE;

        switch (code) {
        case 0:
            reason = Notification::ACTIVATED;
            slotNotificationActionInvoked(notification);
            break;
        case 1:
            //hidden;
            break;
        case 2:
            reason = Notification::DISMISSED;
            break;
        case 3:
            reason = Notification::TIMED_OUT;
            break;
        case -1:
            //failed
            snoreDebug(SNORE_WARNING) << "SnoreToast failed to display " << notification;
            break;
        }

        closeNotification(notification, reason);
    });
    p->start(QLatin1String("SnoreToast"), arguements);
}

void SnoreToast::slotRegisterApplication(const Application &application)
{
    if (!application.constHints().contains("windows-app-id")) {
        snoreDebug(SNORE_INFO) << "No windows-app-id found in hints. Installing default shortcut with appID.";
        QProcess *p = createProcess(Notification());
        QStringList arguements;
        arguements << QLatin1String("-install")
                   << QLatin1String("SnoreNotify\\") + qApp->applicationName()
                   << QDir::toNativeSeparators(qApp->applicationFilePath())
                   << appId(application);
        snoreDebug(SNORE_DEBUG) << "SnoreToast" << arguements;
        p->start(QLatin1String("SnoreToast"), arguements);
    }
}

void SnoreToast::slotCloseNotification(Notification notification)
{
    QProcess *p = createProcess(notification);

    QStringList arguements;
    arguements << QLatin1String("-close")
               << QString::number(notification.id());
    snoreDebug(SNORE_DEBUG) << "SnoreToast" << arguements;
    p->start(QLatin1String("SnoreToast"), arguements);
}

QString SnoreToast::appId(const Application &application)
{
    QString appID = application.constHints().value("windows-app-id").toString();
    if (appID.isEmpty()) {
        appID = QString(qApp->organizationName() + QLatin1Char('.') + qApp->applicationName() + QLatin1String(".SnoreToast")).remove(QLatin1Char(' '));
    }
    return appID;
}

QProcess *SnoreToast::createProcess(Notification noti)
{
    QProcess *p = new QProcess(this);
    p->setReadChannelMode(QProcess::MergedChannels);

    connect(p, static_cast<void (QProcess::*)(int, QProcess::ExitStatus)>(&QProcess::finished), [p](int, QProcess::ExitStatus) {
        snoreDebug(SNORE_DEBUG) << p->readAll();
        p->deleteLater();
    });

    connect(p, static_cast<void (QProcess::*)(QProcess::ProcessError)>(&QProcess::error), [this, p, noti](QProcess::ProcessError) {
        setErrorString(name() + p->errorString());
        snoreDebug(SNORE_DEBUG) << p->readAll();
        if (noti.isValid()) {
            closeNotification(noti, Notification::NONE);
        }
        p->deleteLater();
    });
    connect(qApp, &QApplication::aboutToQuit, p, &QProcess::kill);
    return p;
}
