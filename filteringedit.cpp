#include "filteringedit.h"
#include <QDebug>

FilteringEdit::FilteringEdit(QWidget *parent) :
    QTextEdit(parent)
{
}

void FilteringEdit::keyPressEvent(QKeyEvent *e)
{
    int key = e->key();
    Qt::KeyboardModifiers m = e->modifiers();
    const Qt::KeyboardModifiers generalMods = Qt::ShiftModifier | Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier;

    if (m == 0) {
        if (key == Qt::Key_Up) {
            emit prevEntry();
            return;
        }
        if (key == Qt::Key_Down) {
            emit nextEntry();
            return;
        }
        if (key == Qt::Key_PageDown) {
            emit nextPageOfEntries();
            return;
        }
        if (key == Qt::Key_PageUp) {
            emit prevPageOfEntries();
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

    if (m == Qt::ControlModifier) {
        if (key == Qt::Key_Home) {
            emit firstEntry();
            return;
        }
        if (key == Qt::Key_End) {
            emit lastEntry();
            return;
        }
    }

    QTextEdit::keyPressEvent(e);
}
