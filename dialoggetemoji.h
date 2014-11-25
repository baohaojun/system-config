#ifndef DIALOGGETEMOJI_H
#define DIALOGGETEMOJI_H

#include <QDialog>
#include "emojimodel.h"

namespace Ui {
class DialogGetEmoji;
}

class DialogGetEmoji : public QDialog
{
    Q_OBJECT

public:
    explicit DialogGetEmoji(QWidget *parent = 0);
    ~DialogGetEmoji();

private slots:
    void on_emojiFilter_textChanged();

    void on_emojiListView_doubleClicked(const QModelIndex &index);
    void selectedEmojiNoHistory(const QModelIndex &index);
 signals:
    void emojiSelected(const QString&, const QString&);

private:
    Ui::DialogGetEmoji *ui;

    EmojiModel* mEmojiModel;
    bool updateEmojiHistory;
};

#endif // DIALOGGETEMOJI_H
