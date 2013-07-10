#include "snoretoast.h"
#include "core/snore.h"
#include "core/plugins/plugins.h"
#include "core/plugins/snorebackend.h"

#include <QDebug>
#include <QDir>
#include <QProcess>
#include <QSysInfo>

#include <windows.h>

using namespace Snore;

Q_EXPORT_PLUGIN2(snoretoast,SnoreToast)


SnoreToast::SnoreToast():
    SnoreBackend("SnoreToast")
{
}

SnoreToast::~SnoreToast()
{

}

bool SnoreToast::init(SnoreCore *snore)
{
    if(QSysInfo::windowsVersion() != QSysInfo::WV_WINDOWS8)
    {
        return false;
    }
    m_appID = QString("%1.%2.SnoreToast").arg(qApp->organizationName(), qApp->applicationName());

    QProcess *p = new QProcess(this);

    QStringList arguements;
    arguements << "-install"
               << QString("SnoreNotify\\%1").arg(qApp->applicationName())
               << qApp->applicationFilePath()
               << m_appID;
    qDebug() << "SnoreToast" << arguements;
    p->start("SnoreToast", arguements);
    p->waitForFinished();
    if(p->exitCode() != 0)
        return false;

    return SnoreBackend::init(snore);
}

bool SnoreToast::canCloseNotification()
{
    return false;
}

void SnoreToast::slotRegisterApplication(Application *application)
{
    Q_UNUSED(application)
}

void SnoreToast::slotUnregisterApplication(Application *application)
{
    Q_UNUSED(application)
}

void SnoreToast::slotNotify(Notification notification)
{
    QProcess *p = new QProcess(this);

    connect(p,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(slotToastNotificationClosed(int,QProcess::ExitStatus)));
    connect(qApp,SIGNAL(aboutToQuit()),p,SLOT(kill()));

    QStringList arguements;
    arguements << "-t"
               << Notification::toPlainText(notification.title())
               << "-m"
               << Notification::toPlainText(notification.text())
               << "-p"
                  //               << notification.icon().isLocalFile()?QDir::toNativeSeparators(notification.icon().localUrl()):notification.icon().url()
               << QDir::toNativeSeparators(notification.icon().localUrl())
               << "-w"
               << "-appID"
               << m_appID;
    qDebug() << "SnoreToast" << arguements;
    p->start("SnoreToast", arguements);

    p->setProperty("SNORE_NOTIFICATION_ID",notification.id());
}

void SnoreToast::slotToastNotificationClosed(int code, QProcess::ExitStatus)
{
    QProcess *p = qobject_cast<QProcess*>(sender());
    Notification n = getActiveNotificationByID(p->property("SNORE_NOTIFICATION_ID").toUInt());

    NotificationEnums::CloseReasons::closeReason reason = NotificationEnums::CloseReasons::CLOSED;

    switch(code)
    {
    case 0:
        reason = NotificationEnums::CloseReasons::CLOSED;
        if(!n.actions().isEmpty())
        {
            n.setActionInvoked(n.actions().keys().first());
            snore()->notificationActionInvoked(n);
        }
        break;
    case 1:
        //hidden;
        break;
    case 2:
        reason = NotificationEnums::CloseReasons::DISMISSED;
        break;
    case 3:
        reason = NotificationEnums::CloseReasons::TIMED_OUT;
        break;
    case -1:
        //failed
         qWarning() << "SnoreToast failed to display " << n;
        break;
    }

    closeNotification(n,reason);

}
