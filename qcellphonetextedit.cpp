#include "qcellphonetextedit.h"

QCellPhoneTextEdit::QCellPhoneTextEdit(QWidget* parent) : QPlainTextEdit(parent)
{
}

QCellPhoneTextEdit::~QCellPhoneTextEdit()
{
}

void QCellPhoneTextEdit::keyPressEvent(QKeyEvent *e)
{
    QPlainTextEdit::keyPressEvent(e);
    if (e->key() == Qt::Key_Return || e->key() == Qt::Key_Enter) {
        Qt::KeyboardModifiers m = e->modifiers() & (Qt::ShiftModifier | Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier);
        if (m == Qt::ControlModifier) {
            emit controlEnterPressed();
        }
    }
}
