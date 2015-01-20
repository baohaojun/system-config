#include "fileopenfilter.h"
#include <QFileOpenEvent>
#include <QtSingleApplication>
#include "t1wrenchmainwindow.h"


FileOpenFilter::FileOpenFilter(QObject *parent) :
    QObject(parent)
{
}

bool FileOpenFilter::eventFilter(QObject *obj, QEvent *event)
{
    if (event->type() == QEvent::FileOpen) {
        QFileOpenEvent *fileEvent = static_cast<QFileOpenEvent *>(event);
        QtSingleApplication *a = static_cast<QtSingleApplication *>(obj);
        T1WrenchMainWindow *w = static_cast<T1WrenchMainWindow *>(a->activeWindow());
        w->startTask(fileEvent->file());
        return true;
    } else {
        return QObject::eventFilter(obj, event);
    }
}
