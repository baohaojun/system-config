#include "contactmodel.h"
#include <QDebug>
#include <QFont>
#include <QBrush>
#include <lua.hpp>
#include <QSettings>
#include <QProcessEnvironment>
#include <QDir>
#include <QFile>

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

    int n = luaL_len(L, 1);

    for (int i = 1; i <= n; i++) {
        VCard vcard;
        lua_rawgeti(L, -1, i);

        lua_getfield(L, -1, "FN");
        vcard.setName(QString::fromUtf8(lua_tolstring(L, -1, NULL)));
        lua_settop(L, -2);

        lua_getfield(L, -1, "TEL");

        int nTel = luaL_len(L, -1);
        for (int iTel = 1; iTel <= nTel; iTel++) {
            lua_rawgeti(L, -1, iTel);
            vcard.addTel(QString::fromUtf8(lua_tolstring(L, -1, NULL)));
            lua_settop(L, -2);
        }
        lua_settop(L, -2);

        lua_getfield(L, -1, "EMAIL");
        int nEmail = luaL_len(L, -1);
        for (int iEmail = 1; iEmail <= nEmail, iEmail++) {
            lua_rawgeti(L, -1, iEmail);
            vcard.addEmail(QString::fromUtf8(lua_tolstring(L, -1, NULL)));
            lua_settop(L, -2);
        }
        lua_settop(L, -2);

        mVcards.add(vcard);
    }
    lua_settop(L, 0);
    mHistoryHead = mSettings.value("contact-history-head", QVariant(0)).toInt();
    for (int i = 0; i < 20; i++) {
        updateHistory(mSettings.value(QString().sprintf("contact-history-%d", (mHistoryHead - 1 - i + 20) % 20), QVariant("")).toString());
    }
    setFilter("");
}

int ContactModel::rowCount(const QModelIndex & /*parent */) const
{
    return mFilteredKeys.length();
}

//! [Quoting ModelView Tutorial]
// mymodel.cpp
QVariant ContactModel::data(const QModelIndex &index, int role) const
{
    int row = index.row();

    switch(role) {
    case Qt::DisplayRole:
        return mFilteredKeys[row];
        break;
    case Qt::DecorationRole:
        if (1) { // for the key declaration
            if (row >= mFilteredKeys.size())
                return QVariant();
            QString key = mFilteredKeys[row];
            if (!mContactIconMap.contains(key)) {
                const_cast<ContactModel*>(this)->mContactIconMap[key] = QPixmap(mContactIconPathMap[key]);
            }
            return mContactIconMap[key];
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
    int nOldRows = mFilteredKeys.length();
    mFilteredKeys.clear();

    QStringList split;
    int n = luaL_len(L, -1);
    for (int i = 1; i <= n; i++) {
        lua_rawgeti(L, -1, i);
        split << QString::fromUtf8(lua_tolstring(L, -1, NULL));
        lua_settop(L, -2);
    }
    lua_settop(L, 0);

    for (int i = 0; i < mKeyMap.size(); i++) {
        bool match = 1;
        foreach(const QString& stem, split) {
            if (mKeyMap[i].indexOf(stem, 0, Qt::CaseInsensitive) < 0) {
                match = 0;
                break;
            }
        }
        if (match) {
            mFilteredKeys << mKeyMap[i];
        }
    }

    this->dataChanged(index(0, 0), index(nOldRows, 0));
}

QString ContactModel::getContactText(int i)
{
    if (i >= 0 && i < mFilteredKeys.length()) {
        QString key = mFilteredKeys[i];
        return mContactTextMap[key];
    }
    return "";
}

QString ContactModel::getContactPath(int i)
{
    if (i >= 0 && i < mFilteredKeys.length()) {
        QString key = mFilteredKeys[i];
        return mContactIconPathMap[key];
    }
    return "";
}

void ContactModel::updateHistory(int i)
{
    if (i >= 0 && i < mFilteredKeys.size()) {
        QString key = mFilteredKeys[i];
        updateHistory(key);
        mSettings.setValue(QString().sprintf("contact-history-%d", mHistoryHead++ % 20),
                          QVariant(key));
        mSettings.setValue("contact-history-head", QVariant(mHistoryHead));
    }
}

void ContactModel::updateHistory(QString key)
{
    for (int n = 0; n < mKeyMap.size(); n++) {
        if (mKeyMap[n] == key) {
            for (; n > 0; n--) {
                mKeyMap[n] = mKeyMap[n - 1];
            }
            mKeyMap[0] = key;
            break;
        }
    }
}
