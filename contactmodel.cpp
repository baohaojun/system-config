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
#include "contactmodel.h"
#include <QRegularExpression>
#include <QtAlgorithms>


static bool vCardLess(const VCard& v1, const VCard& v2)
{
    QString v1Pinyin, v2Pinyin;
    if (! v1.mPinyin.isEmpty()) {
        v1Pinyin = v1.mPinyin[0];
    }

    if (! v2.mPinyin.isEmpty()) {
        v2Pinyin = v2.mPinyin[0];
    }

    if (v1Pinyin < v2Pinyin)
        return true;

    if (v1Pinyin == v2Pinyin)
        if (v1.mName < v2.mName)
            return true;

    return false;
}

ContactModel::ContactModel(QObject *parent) :
    FilteringModel(parent),
    mIsWeixin(false),
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
    foreach(const VCard& vcard, mVcards) {
        vcard.mPinyin = getPinyinSpelling(vcard.mName);
    }

    std::sort(mVcards.begin(), mVcards.end(), vCardLess);
    initHistory();
    mInputTextHistory = mSettings.value("contactInputHistory").toStringList();
}

static QString normalizedPhone(const QString& phone) {
    if (phone.size() <= 11) {
        return phone;
    }

    QRegularExpression re("[-+ ]");
    QString res = phone;

    res.replace(re, "");
    if (res.size() > 11) {
        res = res.right(11);
    }
    return res;
}

static void bug(const QString& stem) {
    qDebug() << "bug for " << stem;
}

#define debug_it(x) do {                         \
    if (split.contains("zhang")) {              \
        bug(x);                                \
    }                                           \
} while (0)

void ContactModel::filterSelectedItems(const QStringList& split)
{

    bool mFilterWeixin = false;
    QMap <QString, bool> weixinPhoneMap;
    if (mIsWeixin && QFile("weixin-phones.txt").exists()) {
        mFilterWeixin = true;
        QFile f("weixin-phones.txt");
        if (!f.open(QFile::ReadOnly | QFile::Text)) {
            mFilterWeixin = false;
        } else {
            QTextStream in(&f);
            QString phones = in.readAll();
            foreach (const QString& phone, phones.split("\n")) {
                if (phone.isEmpty()) {
                    continue;
                }
                weixinPhoneMap[phone] = 1;
            }
            f.close();
        }
    }

    foreach(const QString& history, mInputTextHistory) {
        int match = 1;
        QStringList pinyinHistoryList = getPinyinSpelling(history);
        pinyinHistoryList << history;
        foreach(const QString& stem, split) {
            int matched_1 = 0;
            foreach (const QString& p, pinyinHistoryList) {
                if (p.contains(stem)) {
                    matched_1 = 1;
                    break;
                }
            }
            if (! matched_1) {
                match = 0;
                break;
            }
        }

        if (match) {
            SelectedItem si(history, history);
            si.icon = mDefaultAvatar;
            if (!mSelectedItemsRevMap.contains(si.displayText)) {
                mSelectedItems << si;
                mSelectedItemsRevMap[si.displayText] = si;
            }
        }
    }

    foreach (const VCard& vcard, mVcards) {
        QStringList tels = vcard.mTels;
        QStringList mails = vcard.mEmails;
        int match = 1;

        QStringList namePinyin;

        if (mFilterWeixin) {
            QStringList tels2;
            foreach(const QString&tel, tels) {
                if (weixinPhoneMap.contains(normalizedPhone(tel))) {
                    tels2 << normalizedPhone(tel);
                }
            }
            tels = tels2;
        }

        if (tels.isEmpty() && ! mIsMail) {
            match = 0;
        }

        foreach(const QString& stem, split) {
            if (vcard.mName.contains(stem, Qt::CaseInsensitive)) {
                continue;
            }

            if (namePinyin.isEmpty()) {
                namePinyin = getPinyinSpelling(vcard.mName);
            }

            if (matchOneString(namePinyin, stem)) {
                continue;
            }

            // else if (vcard.mEmails.contains("42385513@qq.com")) {
            //     debug_it("hello 1 world: " + vcard.mName + " : "
            //              + namePinyin.join(" ") + " : " + split.join(" x ")
            //              + " : " + vcard.mEmails.join(" e "));
            // }

            if (!mIsMail) { // doing tels
                if (matchOneString(tels, stem)) {
                    tels = filterMatchedStrings(tels, stem);
                    continue;
                }

                if (matchOneString(vcard.mEmails, stem)) {
                    continue;
                }

            } else {
                if (matchOneString(mails, stem)) {
                    // this should be done only if stem does not
                    // matches name.  so this must come after name
                    // compared and not matched.

                    // because otherwise, if a user named zhangshuang,
                    // who has two emails, zhangshuang@XXX and
                    // 123456@qq, and we filter it with "qq zhang",
                    // then the 123456@qq will be filtered out when
                    // zhang matches zhangshuang@XXX.

                    mails = filterMatchedStrings(mails, stem);
                    continue;
                }
                if (matchOneString(vcard.mTels, stem)) {
                    continue;
                }
            }
            match = 0;
            break;
        }

        if (match) {
            QStringList whichList = tels;
            if (mIsMail) {
                whichList = mails;
            }
            foreach (const QString& telOrMail, whichList) {
                SelectedItem si(telOrMail, vcard.mName + " " + telOrMail);
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

void ContactModel::maybeAddTextIntoHistory(const QString& text)
{
    mInputTextHistory.push_front(text);
    while (!mInputTextHistory.isEmpty() && mInputTextHistory.length() > mMaxHistEntries) {
        mInputTextHistory.pop_back();
    }

    mSettings.setValue("contactInputHistory", QVariant(mInputTextHistory));
    mSettings.sync();
}
