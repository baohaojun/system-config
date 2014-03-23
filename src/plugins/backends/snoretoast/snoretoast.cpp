#include "snoretoast.h"
#include "core/snore.h"
#include "core/snore_p.h"
#include "core/plugins/plugins.h"
#include "core/plugins/snorebackend.h"

#include <QDebug>
#include <QDir>
#include <QApplication>
#include <QSysInfo>

#include <windows.h>

using namespace Snore;

Q_EXPORT_PLUGIN2(libsnore_backend_snoretoast,SnoreToast)


SnoreToast::SnoreToast():
    SnoreBackend("Windows 8", true, false)
{
}

SnoreToast::~SnoreToast()
{

}

bool SnoreToast::initialize(SnoreCore *snore)
{
    if(QSysInfo::windowsVersion() < QSysInfo::WV_WINDOWS8)
    {
        snoreDebug( SNORE_DEBUG ) << "SnoreToast does not work on windows" << QSysInfo::windowsVersion();
        return false;
    }
    return SnoreBackend::initialize(snore);
}


void SnoreToast::slotNotify(Notification notification)
{
    QProcess *p = new QProcess(this);
    p->setReadChannelMode(QProcess::MergedChannels);

    connect(p,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(slotToastNotificationClosed(int,QProcess::ExitStatus)));
    connect(qApp,SIGNAL(aboutToQuit()),p,SLOT(kill()));

    QStringList arguements;
    arguements << "-t"
               << Snore::toPlainText(notification.title())
               << "-m"
               << Snore::toPlainText(notification.text());
    if(notification.icon().isValid())
    {
        arguements << "-p"
                   << QDir::toNativeSeparators(notification.icon().localUrl());
    }
    arguements << "-w"
               << "-appID"
               << appId(notification.application())
               << "-id"
               << QString::number(notification.id());
    if(notification.hints().value("silent",true).toBool())
    {
        arguements << "-silent";
    }
    snoreDebug( SNORE_DEBUG ) << "SnoreToast" << arguements;
    p->start("SnoreToast", arguements);

    p->setProperty("SNORE_NOTIFICATION_ID",notification.id());
}

void SnoreToast::slotRegisterApplication(const Application &application)
{
    if(!application.constHints().contains("windows_app_id"))
    {
        QProcess *p = new QProcess(this);
        p->setReadChannelMode(QProcess::MergedChannels);

        QStringList arguements;
        arguements << "-install"
                   << QString("SnoreNotify\\%1").arg(qApp->applicationName())
                   << QDir::toNativeSeparators(qApp->applicationFilePath())
                   << appId(application);
        snoreDebug( SNORE_DEBUG ) << "SnoreToast" << arguements;
        p->start("SnoreToast", arguements);

        connect(p,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(slotToastNotificationClosed(int,QProcess::ExitStatus)));
        connect(qApp,SIGNAL(aboutToQuit()),p,SLOT(kill()));
    }
}

void SnoreToast::slotCloseNotification(Notification notification)
{
    QProcess *p = new QProcess(this);
    p->setReadChannelMode(QProcess::MergedChannels);

    QStringList arguements;
    arguements << "-close"
               << QString::number(notification.id());
    snoreDebug( SNORE_DEBUG ) << "SnoreToast" << arguements;
    p->start("SnoreToast", arguements);

    connect(p,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(slotToastNotificationClosed(int,QProcess::ExitStatus)));
    connect(qApp,SIGNAL(aboutToQuit()),p,SLOT(kill()));
}

void SnoreToast::slotToastNotificationClosed(int code, QProcess::ExitStatus)
{
    QProcess *p = qobject_cast<QProcess*>(sender());
    snoreDebug( SNORE_DEBUG ) << p->readAll();
    if(p->property("SNORE_NOTIFICATION_ID").isNull())
    {
        return;
    }

    Notification n = getActiveNotificationByID(p->property("SNORE_NOTIFICATION_ID").toUInt());
    Notification::CloseReasons reason = Notification::CLOSED;

    switch(code)
    {
    case 0:
        reason = Notification::CLOSED;
        snore()->d()->notificationActionInvoked(n);
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
        snoreDebug( SNORE_WARNING ) << "SnoreToast failed to display " << n;
        break;
    }

    closeNotification(n,reason);

}

QString SnoreToast::appId(const Application &application)
{

    QString appID = application.constHints().value("windows_app_id").toString();
    if(appID.isEmpty())
    {
        appID = QString("%1.%2.SnoreToast").arg(qApp->organizationName(), qApp->applicationName()).remove(" ");
    }
    return appID;
}
