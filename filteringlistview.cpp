#include "filteringlistview.h"
#include "wrench.h"

FilteringListView::FilteringListView(QWidget *parent) :
    QListView(parent)
{
}

void FilteringListView::nextEntry()
{
    changeSelection(1);
}

void FilteringListView::changeSelection(int how)
{
    QModelIndexList ml = selectedIndexes();
    int selected = -1;
    if (!ml.isEmpty()) {
        selected = ml[0].row();
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

void FilteringListView::prevEntry()
{
    changeSelection(-1);
}

void FilteringListView::firstEntry()
{
    changeSelection(-model()->rowCount());
}

void FilteringListView::lastEntry()
{
    changeSelection(model()->rowCount());
}

void FilteringListView::nextPageOfEntries()
{
    changeSelection(this->height() / 50);
}

void FilteringListView::prevPageOfEntries()
{
    changeSelection(this->height() / -50);
}

void FilteringListView::dataChanged(const QModelIndex & topLeft, const QModelIndex & bottomRight, const QVector<int> & roles)
{
    QListView::dataChanged(topLeft, bottomRight, roles);
    this->clearSelection();
    selectionModel()->select(model()->index(0, 0), QItemSelectionModel::Select);
}

void FilteringListView::selectedCurrentEntryWithText(const QString& input)
{
    QModelIndexList ml = selectedIndexes();
    int selected = -1;
    if (!ml.isEmpty()) {
        selected = ml[0].row();
    }

    if (selected < 0) {
        emit selectedCurrentText(input);
    } else {
        emit selectedCurrentEntry(model()->index(selected, 0));
    }
    return;
}

void FilteringListView::selectAllEntries()
{
    for (int i = 0; i < model()->rowCount(); i++) {
        emit selectedCurrentEntryNoHistory(model()->index(i, 0));
    }
}
