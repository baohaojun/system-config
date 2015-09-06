#ifndef PHONESCREENSYNCER_H
#define PHONESCREENSYNCER_H

#include <QObject>
#include <QTimer>

class PhoneScreenSyncer : public QObject
{
    Q_OBJECT
public:
    explicit PhoneScreenSyncer(QObject *parent = 0);

signals:
    void phoneScreenUpdate();

public slots:
    void syncScreen();
    void startSyncing(bool);

private slots:
    void on_applicationStateChanged(Qt::ApplicationState);
private:
    QTimer mTimer;
};

#endif // PHONESCREENSYNCER_H
