#ifndef _ADBSTATETHREAD_H_
#define _ADBSTATETHREAD_H_

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
};

#endif /* _ADBSTATETHREAD_H_ */
