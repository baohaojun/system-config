#ifndef EMOJIMODEL_H
#define EMOJIMODEL_H

#include <QAbstractTableModel>
#include <QAbstractListModel>
#include <QMap>
#include <QPixmap>
#include <QSharedPointer>
#include <lua.hpp>
#include <QSettings>
#include "filteringmodel.h"
#include <QTimer>

class EmojiModel : public FilteringModel
{
    Q_OBJECT
public:
    explicit EmojiModel(QObject *parent = 0);
    void filterSelectedItems(const QStringList& split);
    QString getHistoryName();
private slots:
    void loadAllEmojis();


private:
    QMap<QString, QString> mEmojiIconPathMap;
    QMap<QString, QPixmap> mEmojiIconMap;
    QStringList mEmojis;
    QTimer mLoadEmojiTimer;

signals:

public slots:

};

#endif // EMOJIMODEL_H
