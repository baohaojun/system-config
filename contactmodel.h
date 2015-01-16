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
#include "filteringmodel.h"

class ContactModel : public FilteringModel
{
    Q_OBJECT
public:
    explicit ContactModel(QObject *parent = 0);
    void filterSelectedItems(const QStringList& split);
    QString getHistoryName();
    void setMail(bool isMail) {mIsMail = isMail; setFilter("\t");};

private:
    bool mIsMail;
    QPixmap mDefaultAvatar;
    QList<VCard> mVcards;
    QSettings mSettings;

signals:

public slots:

};

#endif // CONTACTMODEL_H
