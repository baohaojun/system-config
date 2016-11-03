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
public:
    void logToUI(const char *log);
    QString adbQuickInputAm(QString arg);
    ~LuaExecuteThread();
    LuaExecuteThread(QObject* parent = NULL);
    void addScript(QStringList script);
    void quitLua();
    bool isQuit();
    void run();
    QString selectArgs(const QStringList&);
    void selectApps();
    void on_argSelected(const QString& arg);
    void load_mail_heads(const QString& subject, const QString& to, const QString& cc, const QString& bcc, const QString& attachments);
    void setVariableLocked(const QString& name, const QString& val);
    QString getVariableLocked(const QString& name, const QString& defaultVal = "");

private:
    QMutex mVariableMutex;
    QHash<QString, QString> mVariableHash;

signals:
    void gotSomeLog(const QString& key, const QString& val);
    void selectArgsSig(const QStringList&);
    void selectAppsSig();
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
