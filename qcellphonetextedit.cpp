#include <QKeyEvent>
#include "qcellphonetextedit.h"
#include <QDebug>
#include "wrench.h"
#include <QTextBlock>


QCellPhoneTextEdit::QCellPhoneTextEdit(QWidget* parent) :
    QTextEdit(parent),
    mSettings("Smartisan", "Wrench", parent)
{
    L = NULL;

    int savedSize = mSettings.value("textEditFontSize", 0).toInt();

    if (savedSize != 0) {
        QFont newFont(this->font().family(), savedSize);
        this->setFont(newFont);
        resizeImages();
    }
}

QCellPhoneTextEdit::~QCellPhoneTextEdit()
{
}

void QCellPhoneTextEdit::resizeImages()
{
    QTextDocument *doc = document();
    for (QTextBlock blockIt = doc->begin(); blockIt != doc->end(); blockIt = blockIt.next()) {
        for (QTextBlock::iterator it = blockIt.begin(); !(it.atEnd()); ++it) {
            QTextFragment fragment = it.fragment();
            if (fragment.isValid()) {
                if(fragment.charFormat().isImageFormat()) {
                    QTextImageFormat newImageFormat = fragment.charFormat().toImageFormat();
                    int s = this->font().pointSize() + 2;
                    newImageFormat.setWidth(s);
                    newImageFormat.setHeight(s);
                    if (newImageFormat.isValid())
                    {
                        QTextCursor helper = textCursor();
                        helper.setPosition(fragment.position());
                        helper.setPosition(fragment.position() + fragment.length(),
                                           QTextCursor::KeepAnchor);
                        helper.setCharFormat(newImageFormat);
                    }
                }
            }
        }
    }
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

    if (key == Qt::Key_Plus && (m & ~Qt::ShiftModifier) == Qt::ControlModifier) {
        QFont newFont(this->font().family(), this->font().pointSize() + 2);
        this->setFont(newFont);
        resizeImages();
        mSettings.setValue("textEditFontSize", this->font().pointSize());
        return;
    } else if (key == Qt::Key_Minus && (m & ~Qt::ShiftModifier) == Qt::ControlModifier) {
        QFont newFont(this->font().family(), this->font().pointSize() > 10 ? this->font().pointSize() - 2 : this->font().pointSize());
        this->setFont(newFont);
        resizeImages();
        mSettings.setValue("textEditFontSize", this->font().pointSize());
        return;
    }
    if ((key == Qt::Key_8 || key == Qt::Key_Asterisk) && (m & (Qt::AltModifier | Qt::MetaModifier))) {
        emit emojiShortcutPressed();
        return;
    }

    if ((key == Qt::Key_7 || key == Qt::Key_Ampersand) && (m & (Qt::AltModifier | Qt::MetaModifier))) {
        emit phoneCallShortcutPressed();
        return;
    }

    QTextEdit::keyPressEvent(e);
}

void QCellPhoneTextEdit::on_emojiSelected(const QString& emojiPath)
{
    int s = this->font().pointSize() + 2;
    textCursor().insertHtml(QString().sprintf("<img src='%s' width=%d height=%d />", qPrintable(emojiPath), s, s));
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

        int error = luaL_loadstring(L, "wrench = require('wrench')") || lua_pcall(L, 0, 0, 0);
        if (error) {
            prompt_user(QString().sprintf("Error loading emojis: %s", lua_tolstring(L, -1, NULL)));
            lua_close(L);
            L = NULL;
            return text;
        }
    }

    lua_getglobal(L, "wrench");
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
