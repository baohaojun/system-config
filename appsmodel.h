#ifndef APPSMODEL_H
#define APPSMODEL_H

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

class AppsModel : public FilteringModel
{
    Q_OBJECT
public:
    explicit AppsModel(QObject *parent = 0);
    void filterSelectedItems(const QStringList& split);
    QString getHistoryName();

private:
    QPixmap mDefaultAvatar;
    QStringList mAppClasses;
    QMap<QString, QString> mAppPackageMap;
    QMap<QString, QStringList> mAppLabelMap;
    QMap<QString, QPixmap> mAppIconMap;
    QSettings mSettings;

signals:

public slots:

};

#endif // APPSMODEL_H
