#ifndef FILTERINGEDIT_H
#define FILTERINGEDIT_H

#include <QPlainTextEdit>

class FilteringEdit : public QTextEdit
{
    Q_OBJECT
public:
    explicit FilteringEdit(QWidget *parent = 0);
    void setAllowSelectAll(bool allow) {
        m_allowSelectAll = allow;
    }
 private:
    bool m_allowSelectAll;

signals:
    void nextEntry();
    void prevEntry();
    void nextPageOfEntries();
    void prevPageOfEntries();
    void firstEntry();
    void lastEntry();
    void selectedCurrentEntryWithText(QString);
    void getCurrentEntryForEdit();
    void selectedCurrentText(QString);
    void selectAllEntries();
    void shiftSelectCurrentEntry();
public slots:
    void changeCurrentText(const QString&);
private:
    void keyPressEvent(QKeyEvent *);
};

#endif // FILTERINGEDIT_H
