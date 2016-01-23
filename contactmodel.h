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
    void setWeixin(bool isWeixin) {
        mIsWeixin = isWeixin;
        setFilter("\t");
    };

    void maybeAddTextIntoHistory(const QString& text);

private:
    bool mIsMail;
    QStringList mInputTextHistory;

    bool mIsWeixin;
    QPixmap mDefaultAvatar;
    QList<VCard> mVcards;

signals:

public slots:

};

#endif // CONTACTMODEL_H
