/* -*- mode: c++ -*- */
#ifndef _QCELLPHONETEXTEDIT_H_
#define _QCELLPHONETEXTEDIT_H_
#include <QtWidgets/QTextEdit>
#include <lua.hpp>

class QCellPhoneTextEdit : public QTextEdit
{
    Q_OBJECT
public:
    explicit QCellPhoneTextEdit(QWidget *parent = 0);
    ~QCellPhoneTextEdit();
    QString getMyText();

signals:
    void controlEnterPressed();
    void emojiShortcutPressed();

public slots:
    void on_emojiSelected(const QString& emoji, const QString& emojiPath);
private:
    QString replaceImagesWithEmoji(const QString& text, const QString& html);
    void keyPressEvent(QKeyEvent *);
    lua_State* L;
};

#endif /* _QCELLPHONETEXTEDIT_H_ */
