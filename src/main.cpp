
#include "snorenotify.h"

#include <QApplication>

#ifdef HAVE_KDE5
#include "core/version.h"
#include <KAboutData>
#include <KI18n/KLocalizedString>
#endif

int main ( int argc, char *argv[] )
{

    QApplication app ( argc, argv );

#ifdef HAVE_KDE5
    KAboutData about("SnoreNotify",i18n("SnoreNotify"),Snore::Version::version(),
                     i18n("A notification deamon."),KAboutLicense::LGPL_V3, i18n("Copyright (c) 2010-2014 Patrick von Reth <vonreth@kde.org>"));

    KAboutData::setApplicationData(about);
#endif

    SnoreNotify sn;
    return app.exec();
}

