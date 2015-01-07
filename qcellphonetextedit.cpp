#include <QKeyEvent>
#include "qcellphonetextedit.h"
#include <QDebug>
#include "t1wrench.h"


QCellPhoneTextEdit::QCellPhoneTextEdit(QWidget* parent) : QTextEdit(parent)
{
    L = NULL;
}

QCellPhoneTextEdit::~QCellPhoneTextEdit()
{
}

void QCellPhoneTextEdit::keyPressEvent(QKeyEvent *e)
{
    int key = e->key();
    Qt::KeyboardModifiers m = e->modifiers();
    const Qt::KeyboardModifiers generalMods = Qt::ShiftModifier | Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier;

    if (key == Qt::Key_Return || key == Qt::Key_Enter) {
        if ((m & generalMods) == Qt::ControlModifier) {
            emit controlEnterPressed();
            return;
        }
    }

    if ((key == Qt::Key_8 || key == Qt::Key_Asterisk) && (m & (Qt::AltModifier | Qt::MetaModifier))) {
        emit emojiShortcutPressed();
        return;
    }

    if ((key == Qt::Key_7 || key == Qt::Key_Ampersand) && (m & (Qt::AltModifier | Qt::MetaModifier))) {
        emit phoneCallShortcutPressed();
        return;
    }

    static bool last_is_escape = false;
    typedef struct {
        int from;
        Qt::KeyboardModifiers mod_from;
        int to;
        Qt::KeyboardModifiers mod_to;
    } single_keymap_t;

    static single_keymap_t single_map[] = {
        { Qt::Key_B, Qt::ControlModifier, Qt::Key_Left, 0, },
        { Qt::Key_F, Qt::ControlModifier, Qt::Key_Right, 0, },
        { Qt::Key_P, Qt::ControlModifier, Qt::Key_Up, 0, },
        { Qt::Key_N, Qt::ControlModifier, Qt::Key_Down, 0, },
        { Qt::Key_A, Qt::ControlModifier, Qt::Key_Home, 0, },
        { Qt::Key_G, Qt::ControlModifier, Qt::Key_Escape, 0, },
        { Qt::Key_D, Qt::ControlModifier, Qt::Key_Delete, 0, },
        { Qt::Key_E, Qt::ControlModifier, Qt::Key_End, 0, },
        { Qt::Key_Y, Qt::ControlModifier, Qt::Key_Insert, Qt::ShiftModifier, },
        { Qt::Key_V, Qt::ControlModifier, Qt::Key_PageDown, 0, },
        { Qt::Key_V, Qt::AltModifier,     Qt::Key_PageUp, 0, },
        { Qt::Key_E, Qt::ControlModifier, Qt::Key_End, 0, },
        { Qt::Key_B, Qt::AltModifier,     Qt::Key_Left, Qt::ControlModifier, },
        { Qt::Key_F, Qt::AltModifier,     Qt::Key_Right, Qt::ControlModifier, },
        { Qt::Key_B, Qt::AltModifier | Qt::ShiftModifier, Qt::Key_Left, Qt::ControlModifier | Qt::ShiftModifier, },
        { Qt::Key_F, Qt::AltModifier | Qt::ShiftModifier, Qt::Key_Right, Qt::ControlModifier | Qt::ShiftModifier, },
        { Qt::Key_Less, Qt::AltModifier | Qt::ShiftModifier, Qt::Key_Home, Qt::ControlModifier, },
        { Qt::Key_Greater, Qt::AltModifier | Qt::ShiftModifier, Qt::Key_End, Qt::ControlModifier, },
        { Qt::Key_Backspace, Qt::AltModifier, Qt::Key_Backspace, Qt::ControlModifier, },
    };

    typedef struct {
        int from;
        Qt::KeyboardModifiers mod_from;
        struct {
            int to;
            Qt::KeyboardModifiers mod_to;
        } mto[5];
    } multi_keymap_t;

    static multi_keymap_t multi_map[] = {
        { Qt::Key_K, Qt::ControlModifier,
          { { Qt::Key_End, Qt::ShiftModifier, },
            { Qt::Key_Delete, 0, },
          },
        },
    };

    if (last_is_escape) {
        if (key == Qt::Key_Shift || key == Qt::Key_Meta || key == Qt::Key_Control || key == Qt::Key_Alt) {
            true;
        } else {
            last_is_escape = false;
        }
        if (m & Qt::AltModifier) {
            m &= ~Qt::AltModifier;
        } else {
            m |= Qt::AltModifier;
        }
    } else if (m == 0 && key == Qt::Key_Escape) {
        last_is_escape = true;
        return;
    }

    for (size_t i = 0; i < sizeof(single_map) / sizeof(single_map[0]); i++) {
        if (m == single_map[i].mod_from)
        if (key == single_map[i].from && m == single_map[i].mod_from) {
            QKeyEvent nkey = QKeyEvent(e->type(), single_map[i].to, single_map[i].mod_to);
            QTextEdit::keyPressEvent(&nkey);
            return;
        }
    }
    for (size_t i = 0; i < sizeof(multi_map) / sizeof(multi_map[0]); i++) {
        if (key == multi_map[i].from && m == multi_map[i].mod_from) {
            for (int j = 0; multi_map[i].mto[j].to; j++) {
                QKeyEvent nkey = QKeyEvent(e->type(), multi_map[i].mto[j].to, multi_map[i].mto[j].mod_to);
                QTextEdit::keyPressEvent(&nkey);
            }
            return;
        }
    }
    QTextEdit::keyPressEvent(e);
}

void QCellPhoneTextEdit::on_emojiSelected(const QString& emoji, const QString& emojiPath)
{
    textCursor().insertHtml(QString().sprintf("<img src='%s' width=16 height=16 />", qPrintable(emojiPath)));
}

QString QCellPhoneTextEdit::getMyText()
{
    QString html = toHtml();
    QString text = toPlainText();
    return replaceImagesWithEmoji(text, html);
}

QString QCellPhoneTextEdit::replaceImagesWithEmoji(const QString& text, const QString& html)
{
    if (! L) {
        L = luaL_newstate();
        luaL_openlibs(L);        /* opens the standard libraries */

        int error = luaL_loadstring(L, "t1wrench = require('t1wrench')") || lua_pcall(L, 0, 0, 0);
        if (error) {
            prompt_user(QString().sprintf("Error loading emojis: %s", lua_tolstring(L, -1, NULL)));
            lua_close(L);
            L = NULL;
            return text;
        }
    }

    lua_getglobal(L, "t1wrench");
    lua_getfield(L, -1, "replace_img_with_emoji");
    lua_pushstring(L, text.toUtf8().constData());
    lua_pushstring(L, html.toUtf8().constData());
    int error = lua_pcall(L, 2, 1, 0);
    if (error) {
        prompt_user(QString().sprintf("Error img -> emojis: %s", lua_tolstring(L, -1, NULL)));
        L = NULL;
        return text;
    }
    QString res = QString::fromUtf8(lua_tolstring(L, -1, NULL));
    lua_settop(L, 0);
    return res;
}
