#ifndef PHONESCREENSYNCER_H
#define PHONESCREENSYNCER_H

#include <QObject>

class PhoneScreenSyncer : public QObject
{
    Q_OBJECT
public:
    explicit PhoneScreenSyncer(QObject *parent = 0);

signals:

public slots:
};

#endif // PHONESCREENSYNCER_H
