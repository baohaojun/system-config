#include "notificationmodel.h"
#include <QDebug>
#include <QFont>
#include <QBrush>
#include <lua.hpp>
#include <QSettings>
#include <QProcessEnvironment>
#include <QDir>
#include <QFile>
#include "selectoutput.h"
#include "vcard.h"
#include "bhj_help.hpp"
#include <QByteArray>
#include <QPixmap>
#include <algorithm>
#include <QTextStream>
#include <QDateTime>

NotificationModel::NotificationModel(QObject *parent) :
    FilteringModel(parent),
    mDefaultAvatar("emojis/iphone-emoji/WRENCH.png")
{
    if (QFile("apps.info").exists()) {
        QFile appsFile("apps.info");
        if (appsFile.open(QFile::ReadOnly | QFile::Text)) {
            QTextStream in(&appsFile);
            QString lines = in.readAll();
            foreach (const QString& line, lines.split("\n")) {
                QStringList info = line.split("=");
                if (info.size() == 3) {
                    mAppIconMap[info[1]] = QPixmap(info[0] + ".png").scaled(48, 48);
                }
            }
        }
    }
    initHistory();
}

void NotificationModel::filterSelectedItems(const QStringList& split)
{
    QStringList keys = sNotifications.keys();
    keys.sort();
    foreach (const QString& key, keys) {
        int match = 1;
        foreach (const QString& stem, split) {
            int stem_match = 0;
            if (sNotifications[key].title.contains(stem, Qt::CaseInsensitive)) {
                stem_match = 1;
            }
            if (! stem_match) {
                match = 0;
                break;
            }
        }

        if (match) {
            SelectedItem si(key, sNotifications[key].title + "\n" + sNotifications[key].text);
            si.icon = mAppIconMap[sNotifications[key].pkg];
            if (!mSelectedItemsRevMap.contains(si.displayText)) {
                mSelectedItems << si;
                mSelectedItemsRevMap[si.displayText] = si;
            }
        }
    }
}


QString NotificationModel::getHistoryName()
{
    return "notification-history";
}

QMap<QString, NotificationModel::Notification> NotificationModel::sNotifications;
void NotificationModel::insertNotification(const QString& aKey, const QString& aPkg, const QString& aTitle, const QString& aText)
{
    sNotifications.insert((
                              QString("") +
                              QString::asprintf("%d", QDateTime::currentMSecsSinceEpoch()) +
                              QString("|") +
                              aKey),
                          Notification(aKey, aPkg, aTitle, aText));
}
