#ifndef EMOJILISTVIEW_H
#define EMOJILISTVIEW_H

#include <QListView>

class EmojiListView : public QListView
{
    Q_OBJECT
public:
    explicit EmojiListView(QWidget *parent = 0);

signals:
    void selectedEmoji(const QModelIndex&);
    void selectedEmojiNoHistory(const QModelIndex&);

public slots:
    void nextEmoji();
    void prevEmoji();
    void firstEmoji();
    void lastEmoji();
    void nextPageEmoji();
    void prevPageEmoji();
    void selectedEmoji();
    void selectAllEmojis();

protected slots:
    void dataChanged(const QModelIndex & topLeft, const QModelIndex & bottomRight, const QVector<int> & roles = QVector<int> ());

private:
    void changeEmoji(int how);
};

#endif // EMOJILISTVIEW_H
