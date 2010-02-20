#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QtGui/QMainWindow>
#include "application.h"
#include "dbusbinding.h"
#include <QStandardItemModel>

namespace Ui
{
    class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = 0);
    ~MainWindow();
    ApplicationsList applicationList;
     QStandardItemModel* itemsList_applications;
     QStandardItemModel* itemsList_alerts;
public slots:
    void applicationListChanged(const ApplicationsList &apls);

private:
    Ui::MainWindow *ui;
    QString currentSelectedApplication;

private slots:
    void on_listView_AlertList_clicked(QModelIndex index);
    void on_listView_applicationList_clicked(QModelIndex index);
};

#endif // MAINWINDOW_H
