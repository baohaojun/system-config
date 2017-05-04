#include "appsmodel.h"
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
#include "wrench.h"

AppsModel::AppsModel(QObject *parent) :
    FilteringModel(parent),
    mDefaultAvatar("emojis/iphone-emoji/WRENCH.png")
{
    mAppClasses.clear();

    if (gDataDir.exists("apps.info")) {
        QFile appsFile(gDataDir.absoluteFilePath("apps.info"));
        if (appsFile.open(QFile::ReadOnly | QFile::Text)) {
            QTextStream in(&appsFile);
            in.setCodec("UTF-8");
            QString lines = in.readAll();
            foreach (const QString& line, lines.split("\n")) {
                QStringList info = line.split("=");
                if (info.size() == 3) {
                    mAppClasses.push_back(info[0]);
                    mAppPackageMap[info[0]] = info[1];
                    mAppLabelMap[info[0]] = QStringList() << info[2] << getPinyinSpelling(info[2]);
                    mAppIconMap[info[0]] = QPixmap(gDataDir.absoluteFilePath(info[0] + ".png")).scaled(48, 48);
                }
            }
        }
    }
    initHistory();
}

void AppsModel::filterSelectedItems(const QStringList& split)
{
    foreach (const QString& app, mAppClasses) {
        int match = 1;
        foreach (const QString& stem, split) {
            int stem_match = 0;
            foreach(const QString& label, mAppLabelMap[app]) {
                if (label.contains(stem, Qt::CaseInsensitive)) {
                    stem_match = 1;
                    break;
                }
            }
            if (! stem_match) {
                match = 0;
                break;
            }
        }


        if (match) {
            SelectedItem si(app, mAppLabelMap[app][0]);
            si.icon = mAppIconMap[app];
            if (!mSelectedItemsRevMap.contains(si.displayText)) {
                mSelectedItems << si;
                mSelectedItemsRevMap[si.displayText] = si;
            }
        }
    }
}


QString AppsModel::getHistoryName()
{
    return "apps-history";
}
