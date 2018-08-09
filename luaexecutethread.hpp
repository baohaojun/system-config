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
    void insertText(const QString&);
    void clickNotification(const QStringList&);
    void selectApps();
    void showNotifications();
    void on_argSelected(const QString& arg);
    void load_mail_heads(const QString& subject, const QString& to, const QString& cc, const QString& bcc, const QString& attachments);
    void setVariableLocked(const QString& name, const QString& val);
    QString getVariableLocked(const QString& name, const QString& defaultVal = "");
    void setProcessVarLocked(const QString& name, const QString& val);
    QString getProcessVarLocked(const QString& name, const QString& defaultVal = "");
    void gotUiTask(const QStringList&);

private:
    QMutex mVariableMutex;
    QHash<QString, QString> mVariableHash;
    static QHash<QString, QString> mProcessVarHash;
    QStringList ignoreOkScripts;

signals:
    void gotSomeLog(const QString& key, const QString& val);
    void gotUiTaskSig(const QString&, const QStringList&);
    void selectArgsSig(const QStringList&);
    void insertTextSig(const QString&);
    void showNotificationsSig();
    void selectAppsSig();
    void sigClickNotification(const QString& key);
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
    QTcpSocket* wrenchSock;
};

#endif /* _LUAEXECUTETHREAD_H_ */
