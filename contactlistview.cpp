#include "contactlistview.h"
#include "t1wrench.h"

ContactListView::ContactListView(QWidget *parent) :
    QListView(parent)
{
}

void ContactListView::nextContact()
{
    changeContact(1);
}

void ContactListView::changeContact(int how)
{
    QModelIndexList ml = selectedIndexes();
    int selected = 0;
    if (!ml.isEmpty()) {
        selected = ml[0].row();
    }

    if (how == 0) {
        emit selectedContact(model()->index(selected, 0));
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

void ContactListView::prevContact()
{
    changeContact(-1);
}

void ContactListView::firstContact()
{
    changeContact(-model()->rowCount());
}

void ContactListView::lastContact()
{
    changeContact(model()->rowCount());
}

void ContactListView::nextPageContact()
{
    changeContact(this->height() / 50);
}

void ContactListView::prevPageContact()
{
    changeContact(this->height() / -50);
}

void ContactListView::dataChanged(const QModelIndex & topLeft, const QModelIndex & bottomRight, const QVector<int> & roles)
{
    QListView::dataChanged(topLeft, bottomRight, roles);
    this->clearSelection();
    selectionModel()->select(model()->index(0, 0), QItemSelectionModel::Select);
}

void ContactListView::selectedContact()
{
    changeContact(0);
}

void ContactListView::selectAllContacts()
{
    for (int i = 0; i < model()->rowCount(); i++) {
        emit selectedContactNoHistory(model()->index(i, 0));
    }
}
