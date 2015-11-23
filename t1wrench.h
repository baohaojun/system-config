#ifndef _T1WRENCH_H_
#define _T1WRENCH_H_
#include <stdio.h>
#include <QtCore>
#include <QMessageBox>

extern QString prompt_user(const QString &info, QMessageBox::StandardButtons buttons = QMessageBox::Ok);
void qSystem(QString str);
extern bool gScreenCapJpg;
extern QString configDirPath;


#endif /* _T1WRENCH_H_ */
