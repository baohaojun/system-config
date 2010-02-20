#include "mainwindow.h"
#include "ui_mainwindow.h"


MainWindow::MainWindow(QWidget *parent)
        : QMainWindow(parent), ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    itemsList_applications=new QStandardItemModel(this);
    itemsList_alerts=new QStandardItemModel(this);
    ui->listView_applicationList->setModel(itemsList_applications);
    ui->listView_AlertList->setModel(itemsList_alerts);

    QDBusReply<ApplicationsList> reply= DBusBinding::snoreInterface.call("getApplicationList");
    if(reply.isValid())
        applicationListChanged(reply);
    else
        qDebug()<<reply.error();
    DBusBinding::snoreInterface.connection().connect(DBusBinding::snoreInterface.service(),DBusBinding::snoreInterface.path(),DBusBinding::snoreInterface.interface(),"applicationListChanged",this,SLOT(applicationListChanged(ApplicationsList)));
}

MainWindow::~MainWindow()
{
    delete ui;
}





void MainWindow::applicationListChanged(const ApplicationsList &apls){
    itemsList_applications->setHorizontalHeaderItem(0,new QStandardItem("Registred Applications"));
    applicationList=apls;
    itemsList_applications->clear();
          QStandardItem *item;
    foreach(const QString &s,applicationList.keys()){
        item=new QStandardItem(s);
        item->setEditable(false);
        itemsList_applications->appendRow(item);
    }
    itemsList_alerts->clear();
    if(!currentSelectedApplication.isEmpty()&&applicationList.contains(currentSelectedApplication)){

        foreach(QSharedPointer<Alert> al,applicationList.value(currentSelectedApplication)->alerts.values()){
            item=new QStandardItem(al->name);
            item->setEnabled(al->active);
            item->setEditable(false);
            itemsList_alerts->appendRow(item);
        }
    }
}


void MainWindow::on_listView_applicationList_clicked(QModelIndex index)        
{
    currentSelectedApplication=itemsList_applications->item(index.row(),index.column())->text();
    itemsList_alerts->setHorizontalHeaderItem(0,new QStandardItem("Alerts"));
    itemsList_alerts->setHorizontalHeaderItem(1,new QStandardItem("Active"));
    itemsList_alerts->clear();
    QStandardItem *item;
    foreach(QSharedPointer<Alert> al,applicationList.value(currentSelectedApplication)->alerts.values()){
        item=new QStandardItem(al->name);
        item->setEnabled(al->active);
        itemsList_alerts->appendRow(item);
    }

}

void MainWindow::on_listView_AlertList_clicked(QModelIndex index)
{
    qDebug()<<DBusBinding::snoreInterface.call(QDBus::AutoDetect,"setAlertActive",currentSelectedApplication,itemsList_alerts->item(index.row(),index.column())->text(),!itemsList_alerts->item(index.row(),index.column())->isEnabled());


}
