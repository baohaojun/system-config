#ifndef _ADBSTATETHREAD_H_
#define _ADBSTATETHREAD_H_

#include <QtCore/QThread>
#include <QTcpSocket>
#include <QTimer>
#include "adbclient.h"
class AdbStateThread : public QThread
{
    Q_OBJECT
public:
    AdbStateThread(QObject *parent = 0);
    ~AdbStateThread();

private slots:
    void onInputDataReady();
    void canPingInputServer();
    void timeOut();
    void inputServerFinished();
    void onDisconnected();

private:
    QTimer *mConnectTimer;
    bool mAdbInputFinished;
    AdbClient* mAdbInput;
    QTcpSocket *pingSocket;
    bool pingReplyOK;

signals:
    void adbStateUpdate(const QString& state);
protected:
    void run();
};

#endif /* _ADBSTATETHREAD_H_ */
