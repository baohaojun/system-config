#ifndef CONTACTLISTVIEW_H
#define CONTACTLISTVIEW_H

#include <QListView>

class ContactListView : public QListView
{
    Q_OBJECT
public:
    explicit ContactListView(QWidget *parent = 0);

signals:
    void selectedContact(const QModelIndex&);
    void selectedContactNoHistory(const QModelIndex&);

public slots:
    void nextContact();
    void prevContact();
    void firstContact();
    void lastContact();
    void nextPageContact();
    void prevPageContact();
    void selectedContact();
    void selectAllContacts();

protected slots:
    void dataChanged(const QModelIndex & topLeft, const QModelIndex & bottomRight, const QVector<int> & roles = QVector<int> ());

private:
    void changeContact(int how);
};

#endif // CONTACTLISTVIEW_H
