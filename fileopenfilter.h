#ifndef FILEOPENFILTER_H
#define FILEOPENFILTER_H

#include <QObject>

class FileOpenFilter : public QObject
{
    Q_OBJECT
public:
    explicit FileOpenFilter(QObject *parent = 0);

    bool eventFilter(QObject *obj, QEvent *event);

signals:

public slots:

};

#endif // FILEOPENFILTER_H
