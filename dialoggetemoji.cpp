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
    ui->filteringListView->setModel(mEmojiModel);

    connect(ui->emojiFilter, SIGNAL(nextEntry()), ui->filteringListView, SLOT(nextEntry()));
    connect(ui->emojiFilter, SIGNAL(prevEntry()), ui->filteringListView, SLOT(prevEntry()));
    connect(ui->emojiFilter, SIGNAL(firstEntry()), ui->filteringListView, SLOT(firstEntry()));
    connect(ui->emojiFilter, SIGNAL(lastEntry()), ui->filteringListView, SLOT(lastEntry()));
    connect(ui->emojiFilter, SIGNAL(nextPageOfEntries()), ui->filteringListView, SLOT(nextPageOfEntries()));
    connect(ui->emojiFilter, SIGNAL(prevPageOfEntries()), ui->filteringListView, SLOT(prevPageOfEntries()));
    connect(ui->emojiFilter, SIGNAL(selectedCurrentEntryWithText(QString)), ui->filteringListView, SLOT(selectedCurrentEntryWithText(QString)));
    connect(ui->emojiFilter, SIGNAL(selectAllEntries()), ui->filteringListView, SLOT(selectAllEntries()));
    connect(ui->filteringListView, SIGNAL(selectedCurrentEntry(QModelIndex)), this, SLOT(on_filteringListView_doubleClicked(QModelIndex)));
    connect(ui->filteringListView, SIGNAL(selectedCurrentEntryNoHistory(QModelIndex)), this, SLOT(selectedCurrentEntryNoHistory(QModelIndex)));

    ui->filteringListView->selectionModel()->select(mEmojiModel->index(0, 0), QItemSelectionModel::Select);
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

void DialogGetEmoji::on_filteringListView_doubleClicked(const QModelIndex &index)
{
    int row = index.row();
    QString emojiPath = mEmojiModel->getSelectedText(row);
    emit emojiSelected(emojiPath);
    if (updateEmojiHistory) {
        mEmojiModel->updateHistory(row);
    }
}

void DialogGetEmoji::selectedCurrentEntryNoHistory(const QModelIndex &index)
{
    updateEmojiHistory = false;
    on_filteringListView_doubleClicked(index);
    updateEmojiHistory = true;
}
