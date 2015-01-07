#ifndef CONTACTMODEL_H
#define CONTACTMODEL_H

#include <QAbstractTableModel>
#include <QAbstractListModel>
#include <QMap>
#include <QPixmap>
#include <QSharedPointer>
#include <lua.hpp>
#include <QSettings>
#include "selectoutput.h"
#include "vcard.h"

class ContactModel : public QAbstractListModel
{
    Q_OBJECT
public:
    explicit ContactModel(QObject *parent = 0);
    int rowCount(const QModelIndex &parent = QModelIndex()) const ;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
    void setFilter(QString filter);

private:
    QMap<QString, VCard> mContactMap;

    QMap<QString, bool> mContactIsHistory;
    QList<QString> mContactHistoryList;

    QPixmap mDefaultAvatar;
    QList<SelectedItem> mSelectedItems;
    QList<VCard> mVcards;
    QString mFilter;
    lua_State *L;
    int mHistoryHead;
    QSettings mSettings;

 public:
    QString getContactSelectedText(int i);
    void updateHistory(int i);
    void updateHistory(QString key);
signals:

public slots:

};

#endif // CONTACTMODEL_H
