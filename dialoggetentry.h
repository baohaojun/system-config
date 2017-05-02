#ifndef DIALOGGETENTRY_H
#define DIALOGGETENTRY_H

#include <QDialog>
#include "filteringmodel.h"

namespace Ui {
class DialogGetEntry;
}

class DialogGetEntry : public QDialog
{
    Q_OBJECT

public:
    explicit DialogGetEntry(FilteringModel*, const QString&, QWidget *parent = 0, bool allowSelectAll = false);
    ~DialogGetEntry();
    void setHint(const QString& hint);

private slots:
    void on_entryFilter_textChanged();
    void refreshFilter();

    void on_filteringListView_doubleClicked(const QModelIndex &index);
    void selectedCurrentEntryNoHistory(const QModelIndex &index);
    void on_entrySelected(const QString& entry);
    void on_currentTextSelected(const QString& text);
 signals:
    void entrySelected(const QString&);
    void entrySelectedWithDisplayText(const QString&, const QString&);
    void selectRawData(const QMap<QString, QString>&);

private:
    Ui::DialogGetEntry *ui;

    FilteringModel * mEntryModel;
    bool updateEntryHistory;
};

#endif // DIALOGGETENTRY_H
