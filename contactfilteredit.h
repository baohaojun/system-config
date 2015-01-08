#ifndef CONTACTFILTEREDIT_H
#define CONTACTFILTEREDIT_H

#include <QPlainTextEdit>

class ContactFilterEdit : public QPlainTextEdit
{
    Q_OBJECT
public:
    explicit ContactFilterEdit(QWidget *parent = 0);

signals:
    void nextContact();
    void prevContact();
    void nextPageContact();
    void prevPageContact();
    void firstContact();
    void lastContact();
    void selectedContact(QString);
    void selectAllContacts();
public slots:

private:
    void keyPressEvent(QKeyEvent *);

};

#endif // CONTACTFILTEREDIT_H
