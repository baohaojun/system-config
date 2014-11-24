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
    QTextEdit::keyPressEvent(e);
    if (e->key() == Qt::Key_Return || e->key() == Qt::Key_Enter) {
        Qt::KeyboardModifiers m = e->modifiers() & (Qt::ShiftModifier | Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier);
        if (m == Qt::ControlModifier) {
            emit controlEnterPressed();
        }
    }
}

void QCellPhoneTextEdit::on_emojiSelected(const QString& emoji, const QString& emojiPath)
{
    document()->addResource(QTextDocument::ImageResource, QUrl(emoji), QImage(emojiPath));
    textCursor().insertHtml(QString().sprintf("<img src='%s' width=16 height=16 />", qPrintable(emoji)));

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
