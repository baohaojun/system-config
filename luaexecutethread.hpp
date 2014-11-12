#ifndef _LUAEXECUTETHREAD_H_
#define _LUAEXECUTETHREAD_H_
#include <QtCore/QThread>
#include <QtCore/QStringList>
#include <QtCore/QList>
#include <QtCore/QMutex>
#include <QtCore/QWaitCondition>

class LuaExecuteThread : public QThread
{
    Q_OBJECT
public:
    LuaExecuteThread(QObject* parent = NULL);
    void addScript(QStringList script);
    void quitLua();
    void run();
signals:
    void gotSomeLog(const QString& key, const QString& val);
private:
    bool mQuit;
    QMutex mMutex;
    QList<QStringList> mActions;
    QWaitCondition mWait;
};

#endif /* _LUAEXECUTETHREAD_H_ */
