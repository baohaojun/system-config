#include "dialoggetemoji.h"
#include "ui_dialoggetemoji.h"
#include "emojimodel.h"

DialogGetEmoji::DialogGetEmoji(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::DialogGetEmoji)
{
    ui->setupUi(this);
    ui->emojiFilter->setFocus();
    mEmojiModel = new EmojiModel(0);
    ui->emojiListView->setModel(mEmojiModel);

    connect(ui->emojiFilter, SIGNAL(nextEmoji()), ui->emojiListView, SLOT(nextEmoji()));
    connect(ui->emojiFilter, SIGNAL(prevEmoji()), ui->emojiListView, SLOT(prevEmoji()));
    connect(ui->emojiFilter, SIGNAL(firstEmoji()), ui->emojiListView, SLOT(firstEmoji()));
    connect(ui->emojiFilter, SIGNAL(lastEmoji()), ui->emojiListView, SLOT(lastEmoji()));
    connect(ui->emojiFilter, SIGNAL(nextPageEmoji()), ui->emojiListView, SLOT(nextPageEmoji()));
    connect(ui->emojiFilter, SIGNAL(prevPageEmoji()), ui->emojiListView, SLOT(prevPageEmoji()));
    connect(ui->emojiFilter, SIGNAL(selectedEmoji()), ui->emojiListView, SLOT(selectedEmoji()));
    connect(ui->emojiFilter, SIGNAL(selectAllEmojis()), ui->emojiListView, SLOT(selectAllEmojis()));
    connect(ui->emojiListView, SIGNAL(selectedEmoji(QModelIndex)), this, SLOT(on_emojiListView_doubleClicked(QModelIndex)));
    connect(ui->emojiListView, SIGNAL(selectedEmojiNoHistory(QModelIndex)), this, SLOT(selectedEmojiNoHistory(QModelIndex)));

    ui->emojiListView->selectionModel()->select(mEmojiModel->index(0, 0), QItemSelectionModel::Select);
    updateEmojiHistory = true;
}

DialogGetEmoji::~DialogGetEmoji()
{
    delete ui;
}

void DialogGetEmoji::on_emojiFilter_textChanged()
{
    mEmojiModel->setFilter(ui->emojiFilter->toPlainText());
}

void DialogGetEmoji::on_emojiListView_doubleClicked(const QModelIndex &index)
{
    int row = index.row();
    QString emoji = mEmojiModel->getEmojiText(row);
    QString emojiPath = mEmojiModel->getEmojiPath(row);
    emit emojiSelected(emoji, emojiPath);
    if (updateEmojiHistory) {
        mEmojiModel->updateHistory(row);
    }
}

void DialogGetEmoji::selectedEmojiNoHistory(const QModelIndex &index)
{
    updateEmojiHistory = false;
    on_emojiListView_doubleClicked(index);
    updateEmojiHistory = true;
}
