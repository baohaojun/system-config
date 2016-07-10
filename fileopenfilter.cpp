#include "fileopenfilter.h"
#include <QFileOpenEvent>
#include <QtSingleApplication>
#include "wrenchmainwindow.h"


FileOpenFilter::FileOpenFilter(QObject *parent) :
    QObject(parent)
{
}

bool FileOpenFilter::eventFilter(QObject *obj, QEvent *event)
{
    if (event->type() == QEvent::FileOpen) {
        QFileOpenEvent *fileEvent = static_cast<QFileOpenEvent *>(event);
        QtSingleApplication *a = static_cast<QtSingleApplication *>(obj);
        WrenchMainWindow *w = static_cast<WrenchMainWindow *>(a->activeWindow());
        w->startTask(fileEvent->file());
        return true;
    } else {
        return QObject::eventFilter(obj, event);
    }
}
