#ifndef _LUAEXECUTETHREAD_H_
#define _LUAEXECUTETHREAD_H_
#include <QtCore/QThread>
#include <QtCore/QStringList>
#include <QtCore/QList>
#include <QtCore/QMutex>
#include <QtCore/QWaitCondition>
#include <lua.hpp>
#include <QAbstractSocket>
#include <QTcpSocket>

class LuaExecuteThread : public QThread
{
    Q_OBJECT
public slots:
    void t1SockStateChange(QAbstractSocket::SocketState);

public:
    QString adbQuickInputAm(QString arg);
    LuaExecuteThread(QObject* parent = NULL);
    void addScript(QStringList script);
    void quitLua();
    void run();
    QString selectArgs(const QStringList&);
    void on_argSelected(const QString& arg);
    void load_mail_heads(const QString& subject, const QString& to, const QString& cc, const QString& bcc, const QString& attachments);
signals:
    void gotSomeLog(const QString& key, const QString& val);
    void selectArgsSig(const QStringList&);
    void load_mail_heads_sig(const QString& subject, const QString& to, const QString& cc, const QString& bcc, const QString& attachments);
signals:
    void requestSyncScreen();
private:
    lua_State *L;
    QString mSelectedArg;
    bool mQuit;
    QMutex mMutex;
    QList<QStringList> mActions;
    QWaitCondition mWait;
    QWaitCondition mSelectArgsWait;
    QMutex mSelectArgsMutex;
    QTcpSocket* t1Sock;
};

#endif /* _LUAEXECUTETHREAD_H_ */
