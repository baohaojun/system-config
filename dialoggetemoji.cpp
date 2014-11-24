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
}
