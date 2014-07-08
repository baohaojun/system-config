/********************************************************************************
** Form generated from reading UI file 'promptdialog.ui'
**
** Created by: Qt User Interface Compiler version 5.3.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_PROMPTDIALOG_H
#define UI_PROMPTDIALOG_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>

QT_BEGIN_NAMESPACE

class Ui_PromptDialog
{
public:
    QDialogButtonBox *buttonBox;
    QLabel *dialogInfo;

    void setupUi(QDialog *PromptDialog)
    {
        if (PromptDialog->objectName().isEmpty())
            PromptDialog->setObjectName(QStringLiteral("PromptDialog"));
        PromptDialog->resize(400, 300);
        buttonBox = new QDialogButtonBox(PromptDialog);
        buttonBox->setObjectName(QStringLiteral("buttonBox"));
        buttonBox->setGeometry(QRect(30, 240, 341, 32));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);
        dialogInfo = new QLabel(PromptDialog);
        dialogInfo->setObjectName(QStringLiteral("dialogInfo"));
        dialogInfo->setGeometry(QRect(20, 20, 351, 191));
        dialogInfo->setAlignment(Qt::AlignCenter);

        retranslateUi(PromptDialog);
        QObject::connect(buttonBox, SIGNAL(accepted()), PromptDialog, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), PromptDialog, SLOT(reject()));

        QMetaObject::connectSlotsByName(PromptDialog);
    } // setupUi

    void retranslateUi(QDialog *PromptDialog)
    {
        PromptDialog->setWindowTitle(QApplication::translate("PromptDialog", "Dialog", 0));
        dialogInfo->setText(QApplication::translate("PromptDialog", "TextLabel", 0));
    } // retranslateUi

};

namespace Ui {
    class PromptDialog: public Ui_PromptDialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PROMPTDIALOG_H
