#include "snoretoast.h"
#include "core/snore.h"
#include "core/plugins/plugins.h"
#include "core/plugins/snorebackend.h"

#include <QDebug>
#include <QDir>
#include <QApplication>
#include <QSysInfo>

#include <windows.h>

using namespace Snore;

Q_EXPORT_PLUGIN2(snoretoast,SnoreToast)


SnoreToast::SnoreToast():
    SnoreBackend("Windows 8",false,false)
{
}

SnoreToast::~SnoreToast()
{

}

bool SnoreToast::init(SnoreCore *snore)
{
    if(QSysInfo::windowsVersion() != QSysInfo::WV_WINDOWS8)
    {
        qDebug() << "SnoreToast does not work on windows" << QSysInfo::windowsVersion();
        return false;
    }
    if(snore->hints().contains("WINDOWS_APP_ID"))
    {
        m_appID = snore->hints().value("WINDOWS_APP_ID").toString();
    }
    else
    {
        m_appID = QString("%1.%2.SnoreToast").arg(qApp->organizationName(), qApp->applicationName()).remove(" ");

        QProcess *p = new QProcess(this);
        p->setReadChannelMode(QProcess::MergedChannels);

        QStringList arguements;
        arguements << "-install"
                   << QString("SnoreNotify\\%1").arg(qApp->applicationName())
                   << QDir::toNativeSeparators(qApp->applicationFilePath())
                   << m_appID;
        qDebug() << "SnoreToast" << arguements;
        p->start("SnoreToast", arguements);
        p->waitForFinished(-1);
        qDebug() << p->readAll();
        if(p->exitCode() != 0)
            return false;
    }

    return SnoreBackend::init(snore);
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
    p->setReadChannelMode(QProcess::MergedChannels);

    connect(p,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(slotToastNotificationClosed(int,QProcess::ExitStatus)));
    connect(qApp,SIGNAL(aboutToQuit()),p,SLOT(kill()));

    QStringList arguements;
    arguements << "-t"
               << Snore::toPlainText(notification.title())
               << "-m"
               << Snore::toPlainText(notification.text())
               << "-p"
                  //               << notification.icon().isLocalFile()?QDir::toNativeSeparators(notification.icon().localUrl()):notification.icon().url()
               << QDir::toNativeSeparators(notification.icon().localUrl())
               << "-w"
               << "-appID"
               << m_appID;
    ;
    if(notification.hints().value("silent",true).toBool())
    {
        arguements << "-silent";
    }
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
        snore()->notificationActionInvoked(n);
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
        qWarning() << "SnoreToast failed to display " << n << p->readAll();
        break;
    }

    closeNotification(n,reason);

}
