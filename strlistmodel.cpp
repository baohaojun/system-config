#include "strlistmodel.h"
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

StrlistModel::StrlistModel(const QStringList& strList, QObject *parent) :
    FilteringModel(parent),
    mDefaultAvatar("emojis/iphone-emoji/WRENCH.png")
{
    mStrList = strList;
    initHistory();
}

void StrlistModel::filterSelectedItems(const QStringList& split)
{
    foreach (const QString& str, mStrList) {
        int match = 1;
        foreach(const QString& stem, split) {
            if (str.contains(stem, Qt::CaseInsensitive)) {
                continue;
            }
            match = 0;
            break;
        }

        if (match) {
            SelectedItem si(str, str);
            if (!mSelectedItemsRevMap.contains(si.displayText)) {
                mSelectedItems << si;
                mSelectedItemsRevMap[si.displayText] = si;
            }
        }
    }
}


QString StrlistModel::getHistoryName()
{
    return "strlist-history";
}
