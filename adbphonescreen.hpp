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
private:
    bool shouldStop;
};

#endif /* _ADBPHONESCREEN_H_ */
