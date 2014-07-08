#ifndef _READINFOTHREAD_H_
#define _READINFOTHREAD_H_
#include <QtCore/QThread>
#include <QtCore/QStringList>
#include <QtCore/QList>
class ReadInfoThread : public QThread
{
    Q_OBJECT
public:
    ReadInfoThread(QObject* parent = NULL);
    void setProgressive();
    void run();
    void setCommandArgsAndReturnKey(const QString& cmd, const QStringList& args, const QString& key, bool verifyRet = true);
    void setCommandArgsAndReturnKey(const QString& cmd, const QString& key, bool verifyRet = true);
    void setMultipleCommandsAndReturnKey(const QList<QStringList>& cmdAndArgs, const QString& key, bool verifyRet = true);
signals:
    void updateMobileInfo(const QString& key, const QString& val);
private:
    QString mCommand;
    QString mReturnKey;
    QStringList mArgs;
    QList<QStringList> mCommandList;
    bool mVerifyRet;
    bool mProgressive;

    void progressiveRun();
};

#endif /* _READINFOTHREAD_H_ */
