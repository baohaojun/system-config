#ifndef _WRENCH_H_
#define _WRENCH_H_
#include <stdio.h>
#include <QtCore>
#include <QMessageBox>
#include "vncmainwindow.h"

extern QString prompt_user(const QString &info, QMessageBox::StandardButtons buttons = QMessageBox::Ok);
void qSystem(QString str);
extern bool gScreenCapJpg;
extern QString configDirPath;
extern VncMainWindow* vncMainWindow;
extern volatile bool gPhoneScreenSyncOn;


#endif /* _WRENCH_H_ */
