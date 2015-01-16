#include "dialoggetentry.h"
#include "ui_dialoggetentry.h"

DialogGetEntry::DialogGetEntry(FilteringModel* model, const QString& hint, QWidget *parent) :
    QDialog(parent),
    ui(new Ui::DialogGetEntry)
{
    ui->setupUi(this);
    ui->entryFilter->setFocus();
    ui->entryFilter->setPlaceholderText(hint);
    mEntryModel = model;
    ui->filteringListView->setModel(mEntryModel);

    connect(ui->entryFilter, SIGNAL(nextEntry()), ui->filteringListView, SLOT(nextEntry()));
    connect(ui->entryFilter, SIGNAL(prevEntry()), ui->filteringListView, SLOT(prevEntry()));
    connect(ui->entryFilter, SIGNAL(firstEntry()), ui->filteringListView, SLOT(firstEntry()));
    connect(ui->entryFilter, SIGNAL(lastEntry()), ui->filteringListView, SLOT(lastEntry()));
    connect(ui->entryFilter, SIGNAL(nextPageOfEntries()), ui->filteringListView, SLOT(nextPageOfEntries()));
    connect(ui->entryFilter, SIGNAL(prevPageOfEntries()), ui->filteringListView, SLOT(prevPageOfEntries()));
    connect(ui->entryFilter, SIGNAL(selectedCurrentEntryWithText(QString)), ui->filteringListView, SLOT(selectedCurrentEntryWithText(QString)));
    connect(ui->entryFilter, SIGNAL(selectAllEntries()), ui->filteringListView, SLOT(selectAllEntries()));
    connect(ui->filteringListView, SIGNAL(selectedCurrentEntry(QModelIndex)), this, SLOT(on_filteringListView_doubleClicked(QModelIndex)));
    connect(ui->filteringListView, SIGNAL(selectedCurrentText(QString)), this, SLOT(on_entrySelected(QString)));
    connect(ui->filteringListView, SIGNAL(selectedCurrentEntryNoHistory(QModelIndex)), this, SLOT(selectedCurrentEntryNoHistory(QModelIndex)));

    ui->filteringListView->selectionModel()->select(mEntryModel->index(0, 0), QItemSelectionModel::Select);
    updateEntryHistory = true;
}

DialogGetEntry::~DialogGetEntry()
{
    delete ui;
}

void DialogGetEntry::on_entryFilter_textChanged()
{
    mEntryModel->setFilter(ui->entryFilter->toPlainText());
}

void DialogGetEntry::on_filteringListView_doubleClicked(const QModelIndex &index)
{
    int row = index.row();
    QString entry = mEntryModel->getSelectedText(row);
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
}

void DialogGetEntry::selectedCurrentEntryNoHistory(const QModelIndex &index)
{
    updateEntryHistory = false;
    on_filteringListView_doubleClicked(index);
    updateEntryHistory = true;
}
