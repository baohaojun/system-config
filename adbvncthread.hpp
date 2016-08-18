#ifndef _ADBVNCTHREAD_H_
#define _ADBVNCTHREAD_H_

#include <QtCore/QThread>
#include <QTcpSocket>
#include <QTimer>
#include "adbclient.h"
class AdbVncThread : public QThread
{
    Q_OBJECT
public:
    AdbVncThread(QObject *parent = 0);
    ~AdbVncThread();

private slots:
    void onInputDataReady();
    void inputServerFinished();
    void onDisconnected();

private:
    QTimer *mConnectTimer;
    bool mAdbInputFinished;
    AdbClient* mAdbInput;

signals:
    void adbVncUpdate(const QString& state);
    void adbVncInfo(const QString& type, const QString& text);
protected:
    void run();
};

#endif /* _ADBVNCTHREAD_H_ */
