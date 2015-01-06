// -*- mode: c++ -*-
#ifndef _SELECTOUTPUT_H_
#define _SELECTOUTPUT_H_
#include <QCoreApplication>
#include <QPixmap>

struct SelectedItem {
    QString selectedText;
    QString displayText;
    QPixmap icon;
    SelectedItem(QString name, QString display) : selectedText(name), displayText(display) {};
};

#endif /* _SELECTOUTPUT_H_ */
