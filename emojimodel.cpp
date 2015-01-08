#include "emojimodel.h"
#include <QDebug>
#include <QFont>
#include <QBrush>
#include <lua.hpp>
#include <QSettings>

EmojiModel::EmojiModel(QObject *parent) :
    QAbstractListModel(parent),
    mSettings("Smartisan", "Wrench", parent)
{
    L = luaL_newstate();             /* opens Lua */
    luaL_openlibs(L);        /* opens the standard libraries */

    int error = luaL_loadstring(L, "emojis = require('emojis')") || lua_pcall(L, 0, 0, 0);
    if (error) {
        qDebug() << "Error loading emojis: " << QString::fromUtf8(lua_tolstring(L, -1, NULL));
        return;
    }

    lua_getglobal(L, "emojis");
    int n = luaL_len(L, 1);
    for (int i = 1; i <= n; i++) {
        lua_rawgeti(L, -1, i);

        lua_rawgeti(L, -1, 1);
        QString text = QString::fromUtf8(lua_tolstring(L, -1, NULL));
        lua_settop(L, -2);

        lua_rawgeti(L, -1, 2);
        QString key = QString::fromUtf8(lua_tolstring(L, -1, NULL));
        key = QString().sprintf("%04d", i) + key;
        lua_settop(L, -2);

        lua_rawgeti(L, -1, 3);
        QString pngPath = QString::fromUtf8(lua_tolstring(L, -1, NULL));
        lua_settop(L, 1); // finished with this small table

        mEmojiTextMap[key] = text;
        mEmojiIconPathMap[key] = pngPath;
        mKeyMap[i - 1] = key;
    }
    lua_settop(L, 0);
    mHistoryHead = mSettings.value("emoji-history-head", QVariant(0)).toInt();
    for (int i = 0; i < 20; i++) {
        updateHistory(mSettings.value(QString().sprintf("emoji-history-%d", (mHistoryHead - 1 - i + 20) % 20), QVariant("")).toString());
    }
    setFilter("");
}

int EmojiModel::rowCount(const QModelIndex & /*parent */) const
{
    return mFilteredKeys.length();
}

//! [Quoting ModelView Tutorial]
// mymodel.cpp
QVariant EmojiModel::data(const QModelIndex &index, int role) const
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
            if (!mEmojiIconMap.contains(key)) {
                const_cast<EmojiModel*>(this)->mEmojiIconMap[key] = QPixmap(mEmojiIconPathMap[key]);
            }
            return mEmojiIconMap[key];
        }
    }
    return QVariant();
}

void EmojiModel::setFilter(QString filter)
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

QString EmojiModel::getEmojiText(int i)
{
    if (i >= 0 && i < mFilteredKeys.length()) {
        QString key = mFilteredKeys[i];
        return mEmojiTextMap[key];
    }
    return "";
}

QString EmojiModel::getEmojiPath(int i)
{
    if (i >= 0 && i < mFilteredKeys.length()) {
        QString key = mFilteredKeys[i];
        return mEmojiIconPathMap[key];
    }
    return "";
}

void EmojiModel::updateHistory(int i)
{
    if (i >= 0 && i < mFilteredKeys.size()) {
        QString key = mFilteredKeys[i];
        updateHistory(key);
        mSettings.setValue(QString().sprintf("emoji-history-%d", mHistoryHead++ % 20),
                          QVariant(key));
        mSettings.setValue("emoji-history-head", QVariant(mHistoryHead));
    }
}

void EmojiModel::updateHistory(QString key)
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
