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
    FilteringModel(parent),
    mDefaultAvatar("emojis/iphone-emoji/WRENCH.png")
{
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
    initHistory();
}

void ContactModel::filterSelectedItems(const QStringList& split)
{
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
                if (!mSelectedItemsRevMap.contains(si.displayText)) {
                    mSelectedItems << si;
                    mSelectedItemsRevMap[si.displayText] = si;
                }
            }
        }
    }
}

QString ContactModel::getHistoryName()
{
    return "contact-history";
}
