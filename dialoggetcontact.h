#ifndef DIALOGGETCONTACT_H
#define DIALOGGETCONTACT_H

#include <QDialog>
#include "contactmodel.h"

namespace Ui {
class DialogGetContact;
}

class DialogGetContact : public QDialog
{
    Q_OBJECT

public:
    explicit DialogGetContact(QWidget *parent = 0);
    ~DialogGetContact();

private slots:
    void on_contactFilter_textChanged();

    void on_contactListView_doubleClicked(const QModelIndex &index);
    void selectedContactNoHistory(const QModelIndex &index);
    void on_contactSelected(const QString& contact);
 signals:
    void contactSelected(const QString&);

private:
    Ui::DialogGetContact *ui;

    ContactModel* mContactModel;
    bool updateContactHistory;
};

#endif // DIALOGGETCONTACT_H
