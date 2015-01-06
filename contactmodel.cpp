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

ContactModel::ContactModel(QObject *parent) :
    QAbstractListModel(parent),
    mSettings("Smartisan", "Wrench", parent)
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
        vcf = "test.vcf";
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
        qDebug() << i;
        VCard vcard;
        qDebug() << __LINE__;
        lua_rawgeti(L, -1, i);

        qDebug() << __LINE__ << lua_gettop(L);

        lua_getfield(L, -1, "FNWrench");
        qDebug() << __LINE__;
        vcard.mName = (QString::fromUtf8(lua_tolstring(L, -1, NULL)));
        qDebug() << "name is " << vcard.mName;
        lua_settop(L, -2);

        lua_getfield(L, -1, "TELS");

        int nTel = luaL_len(L, -1);
        for (int iTel = 1; iTel <= nTel; iTel++) {
            lua_rawgeti(L, -1, iTel);
            vcard.mTels.push_back(QString::fromUtf8(lua_tolstring(L, -1, NULL)));
            qDebug() << "tel is " << vcard.mTels;
            lua_settop(L, -2);
        }
        lua_settop(L, -2);

        lua_getfield(L, -1, "EMAILS");
        int nEmail = luaL_len(L, -1);
        for (int iEmail = 1; iEmail <= nEmail; iEmail++) {
            lua_rawgeti(L, -1, iEmail);
            vcard.mEmails.push_back(QString::fromUtf8(lua_tolstring(L, -1, NULL)));
            qDebug() << "email is " << vcard.mEmails;
            lua_settop(L, -2);
        }
        lua_settop(L, -2);

        lua_getfield(L, -1, "

        lua_settop(L, -2);

        mVcards << vcard;
    }

    lua_settop(L, 0);
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
            SelectedItem si = mSelectedItems[row];
            return si.icon;
        }
    }
    return QVariant();
}

void ContactModel::setFilter(QString filter)
{
    qDebug() << "setFilter: " << filter;
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

    qDebug() << "Got " << mVcards.length() << " vcards";
    if (!mVcards.isEmpty()) {
        qDebug() << "First vcard has " << mVcards[0].mTels.length() << " Telephones";
    }
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
                qDebug() << namePinyin;
                continue;
            }

            match = 0;
            break;
        }


        if (match) {
            foreach (const QString& tel, tels) {
                SelectedItem si(vcard.mName, QString().sprintf("%s %s", qPrintable(vcard.mName), qPrintable(tel)));
                mSelectedItems << si;
            }
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
    //fixme
}

void ContactModel::updateHistory(QString key)
{
    // fixme
}
