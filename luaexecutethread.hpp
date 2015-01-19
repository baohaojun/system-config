#ifndef _LUAEXECUTETHREAD_H_
#define _LUAEXECUTETHREAD_H_
#include <QtCore/QThread>
#include <QtCore/QStringList>
#include <QtCore/QList>
#include <QtCore/QMutex>
#include <QtCore/QWaitCondition>
#include <lua.hpp>

class LuaExecuteThread : public QThread
{
    Q_OBJECT
public:
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
private:
    lua_State *L;
    QString mSelectedArg;
    bool mQuit;
    QMutex mMutex;
    QList<QStringList> mActions;
    QWaitCondition mWait;
    QWaitCondition mSelectArgsWait;
    QMutex mSelectArgsMutex;
};

#endif /* _LUAEXECUTETHREAD_H_ */
