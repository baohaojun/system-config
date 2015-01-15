#ifndef FILTERINGEDIT_H
#define FILTERINGEDIT_H

#include <QPlainTextEdit>

class FilteringEdit : public QPlainTextEdit
{
    Q_OBJECT
public:
    explicit FilteringEdit(QWidget *parent = 0);

signals:
    void nextEntry();
    void prevEntry();
    void nextPageOfEntries();
    void prevPageOfEntries();
    void firstEntry();
    void lastEntry();
    void selectedCurrentEntryWithText(QString);
    void selectAllEntries();
public slots:

private:
    void keyPressEvent(QKeyEvent *);

};

#endif // FILTERINGEDIT_H
