#include "emojilistview.h"
#include "t1wrench.h"

EmojiListView::EmojiListView(QWidget *parent) :
    QListView(parent)
{
}

void EmojiListView::nextEmoji()
{
    changeEmoji(1);
}

void EmojiListView::changeEmoji(int how)
{
    QModelIndexList ml = selectedIndexes();
    int selected = 0;
    if (!ml.isEmpty()) {
        selected = ml[0].row();
    }

    if (how == 0) {
        emit selectedEmoji(model()->index(selected, 0));
        return;
    }

    selected = selected + how;
    if (selected < 0) {
        selected = 0;
    } else if (selected >= model()->rowCount()) {
        selected = model()->rowCount() - 1;
    }

    if (selected >= 0 && selected < model()->rowCount()) {
        this->clearSelection();
        selectionModel()->select(model()->index(selected, 0), QItemSelectionModel::Select);
        scrollTo(model()->index(selected, 0));
    }
}

void EmojiListView::prevEmoji()
{
    changeEmoji(-1);
}

void EmojiListView::firstEmoji()
{
    changeEmoji(-model()->rowCount());
}

void EmojiListView::lastEmoji()
{
    changeEmoji(model()->rowCount());
}

void EmojiListView::nextPageEmoji()
{
    changeEmoji(this->height() / 50);
}

void EmojiListView::prevPageEmoji()
{
    changeEmoji(this->height() / -50);
}

void EmojiListView::dataChanged(const QModelIndex & topLeft, const QModelIndex & bottomRight, const QVector<int> & roles)
{
    QListView::dataChanged(topLeft, bottomRight, roles);
    this->clearSelection();
    selectionModel()->select(model()->index(0, 0), QItemSelectionModel::Select);
}

void EmojiListView::selectedEmoji()
{
    changeEmoji(0);
}

void EmojiListView::selectAllEmojis()
{
    for (int i = 0; i < model()->rowCount(); i++) {
        emit selectedEmojiNoHistory(model()->index(i, 0));
    }
}
