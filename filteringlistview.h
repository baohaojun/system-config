#ifndef FILTERINGLISTVIEW_H
#define FILTERINGLISTVIEW_H

#include <QListView>

class FilteringListView : public QListView
{
    Q_OBJECT
public:
    explicit FilteringListView(QWidget *parent = 0);

signals:
    void selectedCurrentEntry(const QModelIndex&);
    void selectedCurrentText(const QString&);
    void selectedCurrentEntryNoHistory(const QModelIndex&);

public slots:
    void nextEntry();
    void prevEntry();
    void firstEntry();
    void lastEntry();
    void nextPageOfEntries();
    void prevPageOfEntries();
    void selectedCurrentEntryWithText(const QString& input = "");
    void selectAllEntries();

protected slots:
    void dataChanged(const QModelIndex & topLeft, const QModelIndex & bottomRight, const QVector<int> & roles = QVector<int> ());

private:
    void changeContact(int how);
};

#endif // FILTERINGLISTVIEW_H
