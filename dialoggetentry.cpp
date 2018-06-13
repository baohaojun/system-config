#include "dialoggetentry.h"
#include "ui_dialoggetentry.h"
#include <QDebug>

DialogGetEntry::DialogGetEntry(FilteringModel* model, const QString& hint, QWidget *parent, bool allowSelectAll) :
    QDialog(parent),
    mIsOneShot(false),
    ui(new Ui::DialogGetEntry)
{
    ui->setupUi(this);
    ui->entryFilter->setFocus();
    ui->entryFilter->setPlaceholderText(hint);
    mEntryModel = model;
    ui->filteringListView->setModel(mEntryModel);

    connect(mEntryModel, SIGNAL(iconsUpdated()), this, SLOT(refreshFilter()));

    connect(ui->entryFilter, SIGNAL(nextEntry()), ui->filteringListView, SLOT(nextEntry()));
    connect(ui->entryFilter, SIGNAL(prevEntry()), ui->filteringListView, SLOT(prevEntry()));
    connect(ui->entryFilter, SIGNAL(firstEntry()), ui->filteringListView, SLOT(firstEntry()));
    connect(ui->entryFilter, SIGNAL(lastEntry()), ui->filteringListView, SLOT(lastEntry()));
    connect(ui->entryFilter, SIGNAL(nextPageOfEntries()), ui->filteringListView, SLOT(nextPageOfEntries()));
    connect(ui->entryFilter, SIGNAL(prevPageOfEntries()), ui->filteringListView, SLOT(prevPageOfEntries()));
    connect(ui->entryFilter, SIGNAL(selectedCurrentEntryWithText(QString)), ui->filteringListView, SLOT(selectedCurrentEntryWithText(QString)));
    connect(ui->entryFilter, SIGNAL(getCurrentEntryForEdit()), ui->filteringListView, SLOT(getCurrentEntryForEdit()));
    connect(ui->filteringListView, SIGNAL(sendCurrentEntryToEdit(QString)), ui->entryFilter, SLOT(changeCurrentText(QString)));
    connect(ui->entryFilter, SIGNAL(selectedCurrentText(QString)), this, SLOT(on_currentTextSelected(QString)));
    if (allowSelectAll)
        ui->entryFilter->setAllowSelectAll(allowSelectAll);
    connect(ui->entryFilter, SIGNAL(selectAllEntries()), ui->filteringListView, SLOT(selectAllEntries()));
    connect(ui->entryFilter, SIGNAL(shiftSelectCurrentEntry()), ui->filteringListView, SLOT(shiftSelectCurrentEntry()));
    connect(ui->filteringListView, SIGNAL(selectRawData(QMap<QString, QString>)), this, SIGNAL(selectRawData(QMap<QString, QString>)));
    connect(ui->filteringListView, SIGNAL(selectedCurrentEntry(QModelIndex)), this, SLOT(on_filteringListView_doubleClicked(QModelIndex)));
    connect(ui->filteringListView, SIGNAL(selectedCurrentText(QString)), this, SLOT(on_currentTextSelected(QString)));
    connect(ui->filteringListView, SIGNAL(selectedCurrentEntryNoHistory(QModelIndex)), this, SLOT(selectedCurrentEntryNoHistory(QModelIndex)));

    ui->filteringListView->selectionModel()->select(mEntryModel->index(0, 0), QItemSelectionModel::Select);
    updateEntryHistory = true;

    ui->entryFilter->installEventFilter(parent);
}

DialogGetEntry::~DialogGetEntry()
{
    delete ui;
}

void DialogGetEntry::on_entryFilter_textChanged()
{
    mEntryModel->setFilter(ui->entryFilter->toPlainText());
}

void DialogGetEntry::refreshFilter()
{
    qDebug() << "refreshFilter";
    QString entry = ui->entryFilter->toPlainText();
    static int i = 0;
    i++;

    entry = entry + " ";
    if (i % 2) {
        entry = entry + " ";
    }
    mEntryModel->setFilter(entry);
}

void DialogGetEntry::on_filteringListView_doubleClicked(const QModelIndex &index)
{
    int row = index.row();
    QString entry = mEntryModel->getSelectedText(row);
    mEntryModel->on_indexSelected(row);
    QString entryDisplay = mEntryModel->getSelectedDisplayText(row);
    on_entrySelected(entry);
    emit entrySelectedWithDisplayText(entry, entryDisplay);
    if (updateEntryHistory) {
        mEntryModel->updateHistory(row);
    }
}

void DialogGetEntry::on_entrySelected(const QString& entry)
{
    emit entrySelected(entry);
    mSelectedEntry = entry;
    if (mIsOneShot) {
        close();
    }
}

void DialogGetEntry::on_currentTextSelected(const QString& text)
{
    on_entrySelected(text);
    mEntryModel->maybeAddTextIntoHistory(text);
}

void DialogGetEntry::selectedCurrentEntryNoHistory(const QModelIndex &index)
{
    updateEntryHistory = false;
    on_filteringListView_doubleClicked(index);
    updateEntryHistory = true;
}

void DialogGetEntry::setHint(const QString& hint)
{
    ui->entryFilter->setPlaceholderText(hint);
}

void DialogGetEntry::on_filteringListView_pressed(const QModelIndex &index)
{

}
