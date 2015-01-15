#include "dialoggetcontact.h"
#include "ui_dialoggetcontact.h"
#include "contactmodel.h"

DialogGetContact::DialogGetContact(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::DialogGetContact)
{
    ui->setupUi(this);
    ui->contactFilter->setFocus();
    mContactModel = new ContactModel(0);
    ui->filteringListView->setModel(mContactModel);

    connect(ui->contactFilter, SIGNAL(nextEntry()), ui->filteringListView, SLOT(nextEntry()));
    connect(ui->contactFilter, SIGNAL(prevEntry()), ui->filteringListView, SLOT(prevEntry()));
    connect(ui->contactFilter, SIGNAL(firstEntry()), ui->filteringListView, SLOT(firstEntry()));
    connect(ui->contactFilter, SIGNAL(lastEntry()), ui->filteringListView, SLOT(lastEntry()));
    connect(ui->contactFilter, SIGNAL(nextPageOfEntries()), ui->filteringListView, SLOT(nextPageOfEntries()));
    connect(ui->contactFilter, SIGNAL(prevPageOfEntries()), ui->filteringListView, SLOT(prevPageOfEntries()));
    connect(ui->contactFilter, SIGNAL(selectedCurrentEntryWithText(QString)), ui->filteringListView, SLOT(selectedCurrentEntryWithText(QString)));
    connect(ui->contactFilter, SIGNAL(selectAllEntries()), ui->filteringListView, SLOT(selectAllEntries()));
    connect(ui->filteringListView, SIGNAL(selectedCurrentEntry(QModelIndex)), this, SLOT(on_filteringListView_doubleClicked(QModelIndex)));
    connect(ui->filteringListView, SIGNAL(selectedCurrentText(QString)), this, SLOT(on_contactSelected(QString)));
    connect(ui->filteringListView, SIGNAL(selectedCurrentEntryNoHistory(QModelIndex)), this, SLOT(selectedCurrentEntryNoHistory(QModelIndex)));

    ui->filteringListView->selectionModel()->select(mContactModel->index(0, 0), QItemSelectionModel::Select);
    updateContactHistory = true;
}

DialogGetContact::~DialogGetContact()
{
    delete ui;
}

void DialogGetContact::on_contactFilter_textChanged()
{
    mContactModel->setFilter(ui->contactFilter->toPlainText());
}

void DialogGetContact::on_filteringListView_doubleClicked(const QModelIndex &index)
{
    int row = index.row();
    QString contact = mContactModel->getSelectedText(row);
    on_contactSelected(contact);
    if (updateContactHistory) {
        mContactModel->updateHistory(row);
    }
}

void DialogGetContact::on_contactSelected(const QString& contact)
{
    emit contactSelected(contact);
}

void DialogGetContact::selectedCurrentEntryNoHistory(const QModelIndex &index)
{
    updateContactHistory = false;
    on_filteringListView_doubleClicked(index);
    updateContactHistory = true;
}
