#ifndef NOTIFICATIONMODEL_H
#define NOTIFICATIONMODEL_H

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
#include <QtCore/QList>

class NotificationModel : public FilteringModel
{
    Q_OBJECT
public:
    explicit NotificationModel(QObject *parent = 0);
    void filterSelectedItems(const QStringList& split);
    QString getHistoryName();

    class Notification {
    public:
        QString key;
        QString pkg;
        QString title;
        QString text;

        Notification(const QString& aKey, const QString& aPkg, const QString& aTitle, const QString& aText) {
            key = aKey;
            pkg = aPkg;
            title = aTitle;
            text = aText;
        }
        Notification() {};
    };

    virtual QMap<QString, QString> getSelectedRawData(int i);

    static void insertNotification(const QString& aKey, const QString& aPkg, const QString& aTitle, const QString& aText);
    static QMap<QString, QString> lookupNotification(const QString& aKey);

private:

    static QList<Notification> m_saved_notifications;
    QPixmap mDefaultAvatar;
    QStringList mAppClasses;
    QMap<QString, QString> mAppPackageMap;
    QMap<QString, QStringList> mAppLabelMap;
    QMap<QString, QPixmap> mAppIconMap;
    QSettings mSettings;

signals:

public slots:

};

#endif // NOTIFICATIONMODEL_H
