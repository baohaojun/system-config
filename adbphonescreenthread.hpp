#ifndef _ADBPHONESCREEN_H_
#define _ADBPHONESCREEN_H_

#include <QtCore/QThread>
#include "phonescreensyncer.h"
#include "phonescreendialog.h"
class AdbPhoneScreenThread : public QThread
{
    Q_OBJECT
public:
    AdbPhoneScreenThread(QObject *parent = 0);
    ~AdbPhoneScreenThread();

protected:
    void run();
public:
    void stopIt();
    void continueLoop();
    void pauseLoop();
public slots:
    void syncScreen();
signals:
    void dialogShow(bool);
    void requestSyncScreen();
private:
    PhoneScreenSyncer* mScreenSyncer;
    PhoneScreenDialog* mPhoneScreenDialog;
    bool shouldStop;
    bool paused;
};

#endif /* _ADBPHONESCREEN_H_ */










