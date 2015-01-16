#ifndef STRLISTMODEL_H
#define STRLISTMODEL_H

#include <QAbstractTableModel>
#include <QAbstractListModel>
#include <QMap>
#include <QPixmap>
#include <QSharedPointer>
#include <lua.hpp>
#include <QSettings>
#include "selectoutput.h"
#include "vcard.h"
#include "filteringmodel.h"

class StrlistModel : public FilteringModel
{
    Q_OBJECT
public:
    explicit StrlistModel(const QStringList& strList, QObject *parent = 0);
    void filterSelectedItems(const QStringList& split);
    QString getHistoryName();

private:
    QPixmap mDefaultAvatar;
    QStringList mStrList;
    QSettings mSettings;

signals:

public slots:

};

#endif // STRLISTMODEL_H
