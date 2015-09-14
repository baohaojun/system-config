/* -*- mode: c++ -*- */
#ifndef _QCELLPHONETEXTEDIT_H_
#define _QCELLPHONETEXTEDIT_H_
#include <QTextEdit>
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
    void phoneCallShortcutPressed();

public slots:
    void on_emojiSelected(const QString& emojiPath);
private:
    QString replaceImagesWithEmoji(const QString& text, const QString& html);
    void keyPressEvent(QKeyEvent *);
    void resizeImages();
    lua_State* L;
};

#endif /* _QCELLPHONETEXTEDIT_H_ */
