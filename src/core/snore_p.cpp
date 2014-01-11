#include "snore_p.h"
#include "snore.h"
#include "plugins/plugins.h"
#include "plugins/snorebackend.h"
#include "plugins/snorefrontend.h"

#include <QApplication>

using namespace Snore;


QString const SnoreCorePrivate::snoreTMP(){
    static const QString tmp = QString("%1/SnoreNotify/").arg(QDir::temp().path());
    return tmp;
}

const QDir &SnoreCorePrivate::pluginDir(){
    static QDir path(QString("%1/snoreplugins").arg(qApp->applicationDirPath()));
    if(!path.exists())
    {
        path = QDir(LIBSNORE_PLUGIN_PATH);
    }
    qDebug() << "PluginDir" << path.absolutePath();
    return path;
}


SnoreCorePrivate::SnoreCorePrivate(QSystemTrayIcon *trayIcon):
    m_trayIcon(trayIcon)
{
    QDir home ( snoreTMP() );
    if ( !home.exists() ){
        home.cdUp();
        home.mkdir("SnoreNotify");
    }
}

SnoreCorePrivate::~SnoreCorePrivate()
{

}

void SnoreCorePrivate::notificationActionInvoked(Notification notification) const
{
    Q_Q(const SnoreCore);
    emit const_cast<SnoreCore*>(q)->actionInvoked(notification);
    SnoreFrontend *nf = notification.source();
    if ( nf != NULL )
    {
        nf->actionInvoked ( notification );
    }
}

void SnoreCorePrivate::slotNotificationClosed(Notification n)
{
    Q_Q(SnoreCore);
    emit q->notificationClosed(n);
}

