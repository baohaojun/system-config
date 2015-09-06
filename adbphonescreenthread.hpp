#ifndef _ADBPHONESCREEN_H_
#define _ADBPHONESCREEN_H_

#include <QtCore/QThread>
class AdbPhoneScreenThread : public QThread
{
    Q_OBJECT
public:
    AdbPhoneScreenThread(QObject *parent = 0);
    ~AdbPhoneScreenThread();

signals:
    void phoneScreenUpdate();
protected:
    void run();
public:
    void stopIt();
    void continueLoop();
    void pauseLoop();
    void setAppState(Qt::ApplicationState appState);
private:
    bool shouldStop;
    bool paused;
    Qt::ApplicationState mAppState;
};

#endif /* _ADBPHONESCREEN_H_ */










