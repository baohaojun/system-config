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
#include "wrench.h"

NotificationModel::NotificationModel(QObject *parent) :
    FilteringModel(parent),
    mDefaultAvatar("emojis/iphone-emoji/WRENCH.png")
{
    if (gDataDir.exists("apps.info")) {
        QFile appsFile(gDataDir.absoluteFilePath("apps.info"));
        if (appsFile.open(QFile::ReadOnly | QFile::Text)) {
            QTextStream in(&appsFile);
            QString lines = in.readAll();
            foreach (const QString& line, lines.split("\n")) {
                QStringList info = line.split("=");
                if (info.size() == 3) {
                    mAppIconMap[info[1]] = QPixmap(gDataDir.absoluteFilePath(info[0] + ".png")).scaled(48, 48);
                }
            }
        }
    }
    initHistory();
}

void NotificationModel::filterSelectedItems(const QStringList& split)
{
    foreach (const Notification& n, m_saved_notifications) {
        int match = 1;
        foreach (const QString& stem, split) {
            int stem_match = 0;
            if (n.title.contains(stem, Qt::CaseInsensitive)) {
                stem_match = 1;
            }
            if (! stem_match) {
                match = 0;
                break;
            }
        }

        if (match) {
            SelectedItem si(n.key, n.title + "\n" + n.text);
            si.icon = mAppIconMap[n.pkg];
            if (!mSelectedItemsRevMap.contains(si.displayText)) {
                mSelectedItems << si;
                mSelectedItemsRevMap[si.displayText] = si;
            }
        }
    }
}

QMap<QString, QString> NotificationModel::getSelectedRawData(int i)
{
    QString key = getSelectedText(i);
    return lookupNotification(key);
}

QString NotificationModel::getHistoryName()
{
    return "";
}

QList<NotificationModel::Notification> NotificationModel::m_saved_notifications;
void NotificationModel::insertNotification(const QString& aKey, const QString& aPkg, const QString& aTitle, const QString& aText)
{
    m_saved_notifications.push_front(Notification(aKey, aPkg, aTitle, aText));
    while (m_saved_notifications.length() > 1000) {
        m_saved_notifications.pop_back();
    }
}

QMap<QString, QString> NotificationModel::lookupNotification(const QString& key)
{
    QMap<QString, QString> map;
    foreach (const Notification& n, m_saved_notifications) {
        if (n.key == key) {
            map.insert("key", n.key);
            map.insert("pkg", n.pkg);
            map.insert("title", n.title);
            map.insert("text", n.text);
            return map;
        }
    }
    return map;
}
