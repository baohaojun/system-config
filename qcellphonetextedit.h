/* -*- mode: c++ -*- */
#ifndef _QCELLPHONETEXTEDIT_H_
#define _QCELLPHONETEXTEDIT_H_
#include <QtWidgets/QPlainTextEdit>

class QCellPhoneTextEdit : public QPlainTextEdit
{
    Q_OBJECT
public:
    explicit QCellPhoneTextEdit(QWidget *parent = 0);
    ~QCellPhoneTextEdit();

signals:
    void controlEnterPressed();

private:
    void keyPressEvent(QKeyEvent *);

};

#endif /* _QCELLPHONETEXTEDIT_H_ */
