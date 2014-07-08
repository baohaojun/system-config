#ifndef _ADBSTATETHREAD_H_
#define _ADBSTATETHREAD_H_

#include <QtCore/QThread>
class AdbStateThread : public QThread
{
    Q_OBJECT
public:
    AdbStateThread(QObject *parent = 0);
    ~AdbStateThread();

signals:
    void adbStateUpdate(const QString& state);
protected:
    void run();
};

#endif /* _ADBSTATETHREAD_H_ */
