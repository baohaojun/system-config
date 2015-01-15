#include "filteringedit.h"
#include <QDebug>

FilteringEdit::FilteringEdit(QWidget *parent) :
    QPlainTextEdit(parent)
{
}

void FilteringEdit::keyPressEvent(QKeyEvent *e)
{
    int key = e->key();
    Qt::KeyboardModifiers m = e->modifiers();
    const Qt::KeyboardModifiers generalMods = Qt::ShiftModifier | Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier;

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
        { Qt::Key_A, Qt::ControlModifier, Qt::Key_Home, 0, },
        { Qt::Key_G, Qt::ControlModifier, Qt::Key_Escape, 0, },
        { Qt::Key_D, Qt::ControlModifier, Qt::Key_Delete, 0, },
        { Qt::Key_E, Qt::ControlModifier, Qt::Key_End, 0, },
        { Qt::Key_Y, Qt::ControlModifier, Qt::Key_Insert, Qt::ShiftModifier, },
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
            if (m == 0 && key == Qt::Key_Escape) {
                if (toPlainText() != "") {
                    setPlainText("");
                    return;
                }
            }
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

    if (m == Qt::ControlModifier) {
        if (key == Qt::Key_P) {
            emit prevEntry();
            return;
        }
        if (key == Qt::Key_N) {
            emit nextEntry();
            return;
        }
        if (key == Qt::Key_V) {
            emit nextPageOfEntries();
            return;
        }
    }

    if (key == Qt::Key_Return || key == Qt::Key_Enter) {
        if (m == 0) {
            emit selectedCurrentEntryWithText(this->toPlainText());
            return;
        }
        if (m == Qt::ShiftModifier) {
            emit selectAllEntries();
            return;
        }
    }


    if (m == Qt::AltModifier) {
        if (key == Qt::Key_V) {
            emit prevPageOfEntries();
            return;
        }
    }

    if (m == Qt::AltModifier | Qt::ShiftModifier) {
        if (key == Qt::Key_Less) {
            emit firstEntry();
            return;
        }
        if (key == Qt::Key_Greater) {
            emit lastEntry();
            return;
        }
    }

    for (size_t i = 0; i < sizeof(single_map) / sizeof(single_map[0]); i++) {
        if (m == single_map[i].mod_from)
        if (key == single_map[i].from && m == single_map[i].mod_from) {
            QKeyEvent nkey = QKeyEvent(e->type(), single_map[i].to, single_map[i].mod_to);
            QPlainTextEdit::keyPressEvent(&nkey);
            return;
        }
    }
    for (size_t i = 0; i < sizeof(multi_map) / sizeof(multi_map[0]); i++) {
        if (key == multi_map[i].from && m == multi_map[i].mod_from) {
            for (int j = 0; multi_map[i].mto[j].to; j++) {
                QKeyEvent nkey = QKeyEvent(e->type(), multi_map[i].mto[j].to, multi_map[i].mto[j].mod_to);
                QPlainTextEdit::keyPressEvent(&nkey);
            }
            return;
        }
    }
    QPlainTextEdit::keyPressEvent(e);
}
