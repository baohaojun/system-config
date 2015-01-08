#include "contactmodel.h"
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

bool vCardLess(const VCard& v1, const VCard& v2)
{
    if (v1.mPinyin.isEmpty()) {
        v1.mPinyin = getPinyinSpelling(v1.mName);
    }

    if (v2.mPinyin.isEmpty()) {
        v2.mPinyin = getPinyinSpelling(v2.mName);
    }

    QString v1Pinyin, v2Pinyin;
    if (! v1.mPinyin.isEmpty()) {
        v1Pinyin = v1.mPinyin[0];
    }

    if (! v2.mPinyin.isEmpty()) {
        v2Pinyin = v2.mPinyin[0];
    }

    if (v1Pinyin < v2Pinyin)
        return true;

    if (v1.mName < v2.mName)
        return true;

    return false;
}

ContactModel::ContactModel(QObject *parent) :
    QAbstractListModel(parent),
    mSettings("Smartisan", "Wrench", parent),
    mDefaultAvatar("emojis/iphone-emoji/WRENCH.png")
{
    L = luaL_newstate();             /* opens Lua */
    luaL_openlibs(L);        /* opens the standard libraries */

    int error = luaL_loadstring(L, "contacts = require('contacts')") || lua_pcall(L, 0, 0, 0);
    if (error) {
        qDebug() << "Error loading contacts: " << QString::fromUtf8(lua_tolstring(L, -1, NULL));
        return;
    }

    lua_getglobal(L, "contacts");

#ifdef Q_OS_WIN32
    QString homePath = QProcessEnvironment::systemEnvironment().value("USERPROFILE");
#else
    QString homePath = QProcessEnvironment::systemEnvironment().value("HOME");
#endif

    QDir homeDir = QDir(homePath);
    QString vcf = homeDir.absoluteFilePath(".android/contacts.vcf");
    QFile vcfFile(vcf);
    if (!vcfFile.exists()) {
        if (QFile("contacts.vcf").exists()) {
            vcf = "contacts.vcf";
        } else {
            vcf = "test.vcf";
        }
    }

    lua_getfield(L, -1, "read_vcf");
    lua_pushstring(L, qPrintable(vcf));

    error = lua_pcall(L, 1, 1, 0);
    if (error) {
        qDebug() << "Error: can't read vcf file: " << vcf;
        return;
    }

    int n = luaL_len(L, -1);
    qDebug() << "n is " << n;
    for (int i = 1; i <= n; i++) {
        VCard vcard;
        lua_rawgeti(L, -1, i);

        lua_getfield(L, -1, "FNWrench");
        vcard.mName = (QString::fromUtf8(lua_tolstring(L, -1, NULL)));
        lua_settop(L, -2);

        lua_getfield(L, -1, "TELS");

        int nTel = luaL_len(L, -1);
        for (int iTel = 1; iTel <= nTel; iTel++) {
            lua_rawgeti(L, -1, iTel);
            vcard.mTels.push_back(QString::fromUtf8(lua_tolstring(L, -1, NULL)));
            lua_settop(L, -2);
        }
        lua_settop(L, -2);

        lua_getfield(L, -1, "EMAILS");
        int nEmail = luaL_len(L, -1);
        for (int iEmail = 1; iEmail <= nEmail; iEmail++) {
            lua_rawgeti(L, -1, iEmail);
            vcard.mEmails.push_back(QString::fromUtf8(lua_tolstring(L, -1, NULL)));
            lua_settop(L, -2);
        }
        lua_settop(L, -2);

        lua_getfield(L, -1, "photo_data");
        size_t len = 0;
        const char* photo_data = lua_tolstring(L, -1, &len);
        if (photo_data != NULL && len != 0) {
            QByteArray photoBytes(photo_data, len);

            photoBytes = QByteArray::fromBase64(photoBytes);
            if (!vcard.mAvatar.loadFromData(photoBytes)) {
                qDebug() << "load for " << vcard.mName << " has failed";
            } else {
                vcard.mAvatar = vcard.mAvatar.scaled(48, 48);
            }
            if (vcard.mAvatar.isNull()) {
                qDebug() << "my icon is null";
            }
        } else {
            vcard.mAvatar = mDefaultAvatar;
        }
        lua_settop(L, -2);

        lua_settop(L, -2);

        mVcards << vcard;
    }

    lua_settop(L, 0);

    std::sort(mVcards.begin(), mVcards.end(), vCardLess);
    mHistoryHead = mSettings.value("contact-history-head", QVariant(0)).toInt();
    for (int j = 0; j < 20; j++) {
        int i = 20 - j - 1;
        updateHistory(mSettings.value(QString().sprintf("contact-history-%d", (mHistoryHead - 1 - i + 20) % 20), QVariant("")).toString());
    }
    setFilter("");
}

int ContactModel::rowCount(const QModelIndex & /*parent */) const
{
    return mSelectedItems.length();
}

//! [Quoting ModelView Tutorial]
// mymodel.cpp
QVariant ContactModel::data(const QModelIndex &index, int role) const
{
    int row = index.row();

    switch(role) {
    case Qt::DisplayRole:
        return mSelectedItems[row].displayText;
        break;
    case Qt::DecorationRole:
        if (1) { // for the key declaration
            if (row >= mSelectedItems.size())
                return QVariant();
            return mSelectedItems[row].icon;
        }
    }
    return QVariant();
}

void ContactModel::setFilter(QString filter)
{
    mFilter = filter;
    int error = luaL_loadstring(L, "t1wrench = require('t1wrench')") || lua_pcall(L, 0, 0, 0);
    lua_getglobal(L, "t1wrench");
    lua_getfield(L, -1, "split");
    lua_pushstring(L, " ");
    lua_pushstring(L, mFilter.toUtf8().constData());
    error = lua_pcall(L, 2, 1, 0);
    if (error) {
         qDebug() << "Error calling split:" << QString::fromUtf8(lua_tolstring(L, -1, NULL));
        return;
    }
    int nOldRows = mSelectedItems.length();
    mSelectedItems.clear();

    QStringList split;
    int n = luaL_len(L, -1);
    for (int i = 1; i <= n; i++) {
        lua_rawgeti(L, -1, i);
        split << QString::fromUtf8(lua_tolstring(L, -1, NULL));
        lua_settop(L, -2);
    }
    lua_settop(L, 0);

    QMap<QString, SelectedItem> unDup;
    foreach (const VCard& vcard, mVcards) {
        QStringList tels = vcard.mTels;
        int match = 1;

        QStringList namePinyin;
        foreach(const QString& stem, split) {
            if (matchOneString(tels, stem)) {
                tels = filterMatchedStrings(tels, stem);
                continue;
            }

            if (vcard.mName.indexOf(stem, 0, Qt::CaseInsensitive) >= 0) {
                continue;
            }

            if (matchOneString(vcard.mEmails, stem)) {
                continue;
            }

            if (namePinyin.isEmpty()) {
                namePinyin = getPinyinSpelling(vcard.mName);
            }

            if (matchOneString(namePinyin, stem)) {
                continue;
            }

            match = 0;
            break;
        }

        if (match) {
            foreach (const QString& tel, tels) {
                SelectedItem si(tel, vcard.mName + " " + tel);
                si.icon = vcard.mAvatar;
                if (!mContactMap.contains(si.displayText)) {
                    mContactMap[si.displayText] = vcard;
                }
                if (!unDup.contains(si.displayText)) {
                    mSelectedItems << si;
                    unDup[si.displayText] = si;
                }
            }
        }
    }

    foreach(const QString& history, mContactHistoryList) {
        if (unDup.contains(history)) {
            mSelectedItems.removeOne(unDup[history]);
            mSelectedItems.push_front(unDup[history]);
        }
    }

    this->dataChanged(index(0, 0), index(nOldRows, 0));
}

QString ContactModel::getContactSelectedText(int i)
{

    if (i >= 0 && i < mSelectedItems.length()) {
        SelectedItem si = mSelectedItems[i];
        return si.selectedText;
    }
    return "";
}

void ContactModel::updateHistory(int i)
{
    if (i >= 0 && i < mSelectedItems.size()) {
        QString key = mSelectedItems[i].displayText;
        updateHistory(key);
        mSettings.setValue(QString().sprintf("contact-history-%d", mHistoryHead++ % 20),
                          QVariant(key));
        mSettings.setValue("contact-history-head", QVariant(mHistoryHead));
    }
}

void ContactModel::updateHistory(QString key)
{
    mContactHistoryList.removeOne(key);
    mContactHistoryList.push_back(key);
    while (mContactHistoryList.size() > 20) {
        mContactHistoryList.pop_front();
    }
}
