#ifndef EMOJIFILTEREDIT_H
#define EMOJIFILTEREDIT_H

#include <QPlainTextEdit>

class EmojiFilterEdit : public QPlainTextEdit
{
    Q_OBJECT
public:
    explicit EmojiFilterEdit(QWidget *parent = 0);

signals:
    void nextEmoji();
    void prevEmoji();
    void nextPageEmoji();
    void prevPageEmoji();
    void firstEmoji();
    void lastEmoji();
    void selectedEmoji();
    void selectAllEmojis();
public slots:

private:
    void keyPressEvent(QKeyEvent *);

};

#endif // EMOJIFILTEREDIT_H
