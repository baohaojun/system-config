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

SnoreToast::SnoreToast():
    SnoreBackend("Windows 8", true, false)
{
}

SnoreToast::~SnoreToast()
{

}

bool SnoreToast::initialize()
{
    if (QSysInfo::windowsVersion() < QSysInfo::WV_WINDOWS8) {
        snoreDebug(SNORE_DEBUG) << "SnoreToast does not work on windows" << QSysInfo::windowsVersion();
        return false;
    }
    return SnoreBackend::initialize();
}

void SnoreToast::slotNotify(Notification notification)
{
    QProcess *p = new QProcess(this);
    p->setReadChannelMode(QProcess::MergedChannels);

    connect(p, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(slotToastNotificationClosed(int,QProcess::ExitStatus)));
    connect(qApp, SIGNAL(aboutToQuit()), p, SLOT(kill()));

    QStringList arguements;
    arguements << "-t"
               << Utils::toPlainText(notification.title())
               << "-m"
               << Utils::toPlainText(notification.text());
    if (notification.icon().isValid()) {
        arguements << "-p"
                   << QDir::toNativeSeparators(notification.icon().localUrl());
    }
    arguements << "-w"
               << "-appID"
               << appId(notification.application())
               << "-id"
               << QString::number(notification.id());
    //TODO: could clash woith sound backend
    if (notification.hints().value("silent", true).toBool() || notification.hints().value("sound").isValid()) {
        arguements << "-silent";
    }
    snoreDebug(SNORE_DEBUG) << "SnoreToast" << arguements;
    p->start("SnoreToast", arguements);

    p->setProperty("SNORE_NOTIFICATION", QVariant::fromValue(notification));
}

void SnoreToast::slotRegisterApplication(const Application &application)
{
    if (!application.constHints().contains("windows_app_id")) {
        QProcess *p = new QProcess(this);
        p->setReadChannelMode(QProcess::MergedChannels);

        QStringList arguements;
        arguements << "-install"
                   << QString("SnoreNotify\\%1").arg(qApp->applicationName())
                   << QDir::toNativeSeparators(qApp->applicationFilePath())
                   << appId(application);
        snoreDebug(SNORE_DEBUG) << "SnoreToast" << arguements;
        p->start("SnoreToast", arguements);

        connect(p, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(slotPrintExitStatus(int,QProcess::ExitStatus)));
        connect(qApp, SIGNAL(aboutToQuit()), p, SLOT(kill()));
    }
}

void SnoreToast::slotCloseNotification(Notification notification)
{
    QProcess *p = new QProcess(this);
    p->setReadChannelMode(QProcess::MergedChannels);

    QStringList arguements;
    arguements << "-close"
               << QString::number(notification.id());
    snoreDebug(SNORE_DEBUG) << "SnoreToast" << arguements;
    p->start("SnoreToast", arguements);

    connect(p, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(slotPrintExitStatus(int,QProcess::ExitStatus)));
    connect(qApp, SIGNAL(aboutToQuit()), p, SLOT(kill()));
}

void SnoreToast::slotToastNotificationClosed(int code, QProcess::ExitStatus)
{
    QProcess *p = qobject_cast<QProcess *>(sender());
    snoreDebug(SNORE_DEBUG) << p->readAll();
    snoreDebug(SNORE_DEBUG) << "Exit code:" << code;

    Notification n = p->property("SNORE_NOTIFICATION").value<Notification>();
    Notification::CloseReasons reason = Notification::NONE;

    switch (code) {
    case 0:
        reason = Notification::ACTIVATED;
        SnoreCorePrivate::instance()->notificationActionInvoked(n);
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
        snoreDebug(SNORE_WARNING) << "SnoreToast failed to display " << n;
        break;
    }

    closeNotification(n, reason);
    p->deleteLater();
}

void SnoreToast::slotPrintExitStatus(int, QProcess::ExitStatus)
{
    QProcess *p = qobject_cast<QProcess *>(sender());
    snoreDebug(SNORE_DEBUG) << p->readAll();
    p->deleteLater();
}

QString SnoreToast::appId(const Application &application)
{

    QString appID = application.constHints().value("windows_app_id").toString();
    if (appID.isEmpty()) {
        appID = QString("%1.%2.SnoreToast").arg(qApp->organizationName(), qApp->applicationName()).remove(" ");
    }
    return appID;
}
