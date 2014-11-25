#ifndef EMOJIMODEL_H
#define EMOJIMODEL_H

#include <QAbstractTableModel>
#include <QAbstractListModel>
#include <QMap>
#include <QPixmap>
#include <QSharedPointer>
#include <lua.hpp>
#include <QSettings>

class EmojiModel : public QAbstractListModel
{
    Q_OBJECT
public:
    explicit EmojiModel(QObject *parent = 0);
    int rowCount(const QModelIndex &parent = QModelIndex()) const ;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
    void setFilter(QString filter);

private:
    QString mFilter;
    QMap<QString, QString> mEmojiTextMap;
    QMap<QString, QString> mEmojiIconPathMap;
    QMap<QString, QPixmap> mEmojiIconMap;
    QMap<int, QString> mKeyMap;
    QStringList mFilteredKeys;
    lua_State *L;
    int mHistoryHead;
    QSettings mSettings;

 public:
    QString getEmojiText(int i);
    QString getEmojiPath(int i);
    void updateHistory(int i);
    void updateHistory(QString key);
signals:

public slots:

};

#endif // EMOJIMODEL_H
