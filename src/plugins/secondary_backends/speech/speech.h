#ifndef SPEECH_H
#define SPEECH_H

#include <QObject>

class Speech : public QObject
{
    Q_OBJECT
public:
    explicit Speech(QObject *parent = 0);

signals:

public slots:
};

#endif // SPEECH_H