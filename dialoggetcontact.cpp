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
    ui->contactListView->setModel(mContactModel);

    connect(ui->contactFilter, SIGNAL(nextContact()), ui->contactListView, SLOT(nextContact()));
    connect(ui->contactFilter, SIGNAL(prevContact()), ui->contactListView, SLOT(prevContact()));
    connect(ui->contactFilter, SIGNAL(firstContact()), ui->contactListView, SLOT(firstContact()));
    connect(ui->contactFilter, SIGNAL(lastContact()), ui->contactListView, SLOT(lastContact()));
    connect(ui->contactFilter, SIGNAL(nextPageContact()), ui->contactListView, SLOT(nextPageContact()));
    connect(ui->contactFilter, SIGNAL(prevPageContact()), ui->contactListView, SLOT(prevPageContact()));
    connect(ui->contactFilter, SIGNAL(selectedContact()), ui->contactListView, SLOT(selectedContact()));
    connect(ui->contactFilter, SIGNAL(selectAllContacts()), ui->contactListView, SLOT(selectAllContacts()));
    connect(ui->contactListView, SIGNAL(selectedContact(QModelIndex)), this, SLOT(on_contactListView_doubleClicked(QModelIndex)));
    connect(ui->contactListView, SIGNAL(selectedContactNoHistory(QModelIndex)), this, SLOT(selectedContactNoHistory(QModelIndex)));

    ui->contactListView->selectionModel()->select(mContactModel->index(0, 0), QItemSelectionModel::Select);
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

void DialogGetContact::on_contactListView_doubleClicked(const QModelIndex &index)
{
    int row = index.row();
    QString contact = mContactModel->getContactSelectedText(row);
    emit contactSelected(contact);
    if (updateContactHistory) {
        mContactModel->updateHistory(row);
    }
}

void DialogGetContact::selectedContactNoHistory(const QModelIndex &index)
{
    updateContactHistory = false;
    on_contactListView_doubleClicked(index);
    updateContactHistory = true;
}
