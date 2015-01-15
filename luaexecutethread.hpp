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
    QString selectArgs(const QStringList&);
    void setSelectedArg(const QString& arg);
signals:
    void gotSomeLog(const QString& key, const QString& val);
    void selectArgsSig(const QStringList&);
private:
    QString mSelectedArg;
    bool mQuit;
    QMutex mMutex;
    QList<QStringList> mActions;
    QWaitCondition mWait;
    QWaitCondition mSelectArgsWait;
    QMutex mSelectArgsMutex;
};

#endif /* _LUAEXECUTETHREAD_H_ */
