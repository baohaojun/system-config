#include "emojimodel.h"
#include <QDebug>
#include <QFont>
#include <QBrush>
#include <lua.hpp>
#include <QSettings>
#include <QTimer>

EmojiModel::EmojiModel(QObject *parent) :
    FilteringModel(parent)
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

        lua_rawgeti(L, -1, 2);
        QString key = QString::fromUtf8(lua_tolstring(L, -1, NULL));
        key = QString().sprintf("%04d", i) + key;
        lua_settop(L, -2);

        lua_rawgeti(L, -1, 3);
        QString pngPath = QString::fromUtf8(lua_tolstring(L, -1, NULL));
        lua_settop(L, 1); // finished with this small table

        mEmojiIconPathMap[key] = pngPath;
        mEmojis << key;
    }
    lua_settop(L, 0);
    initHistory();
}

const int maxLoad = 100;
void EmojiModel::loadAllEmojis()
{
    qDebug() << "load all Emojis" << mEmojiIconMap.size();
    int loaded = 0;
    foreach (const QString& emoji, mEmojis) {
        if (!mEmojiIconMap.contains(emoji) && loaded < maxLoad) {
            mEmojiIconMap[emoji] = QPixmap(mEmojiIconPathMap[emoji]);
            loaded++;
        } else if (loaded == maxLoad) {
            QTimer::singleShot(500, this, SLOT(loadAllEmojis()));
            break;
        }
    }

    emit iconsUpdated();
}

void EmojiModel::filterSelectedItems(const QStringList& split)
{
    int loaded = 0;

    QPixmap defaultPixmap;
    foreach (const QString& emoji, mEmojis) {
        bool match = 1;
        foreach(const QString& stem, split) {
            if (!emoji.contains(stem, Qt::CaseInsensitive)) {
                match = 0;
                break;
            }
        }
        if (match) {
            QPixmap emojiPixmap;
            if (!mEmojiIconMap.contains(emoji) && loaded < maxLoad) {
                emojiPixmap = mEmojiIconMap[emoji] = QPixmap(mEmojiIconPathMap[emoji]);
                loaded++;
            } else if (mEmojiIconMap.contains(emoji)) {
                emojiPixmap = mEmojiIconMap[emoji];
            } else {
                if (loaded == maxLoad) {
                    QTimer::singleShot(500, this, SLOT(loadAllEmojis()));
                    loaded++;
                }

                if (defaultPixmap.isNull()) {
                    defaultPixmap = QPixmap("loading.png");
                }
                emojiPixmap = defaultPixmap;
            }
            SelectedItem si(mEmojiIconPathMap[emoji], emoji, emojiPixmap);
            mSelectedItems << si;
            mSelectedItemsRevMap[emoji] = si;
        }
    }
}

QString EmojiModel::getHistoryName()
{
    return "emoji-history";
}
