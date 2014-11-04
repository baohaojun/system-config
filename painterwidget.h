#ifndef PAINTERWIDGET_H
#define PAINTERWIDGET_H

#include <QGraphicsScene>
#include <QPen>
#include <QColor>
#include "painteritem.h"
#include <QtWidgets/QGraphicsSceneMouseEvent>

class PainterWidget : public QGraphicsScene
{
    Q_OBJECT
public:
    explicit PainterWidget(QObject *parent = 0);

public slots:
    void usingPen();
    void usingRect();
    void setColor(QColor color);

protected:
    void mousePressEvent(QGraphicsSceneMouseEvent *event);
    void mouseMoveEvent(QGraphicsSceneMouseEvent *event);
    void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);
signals:

private:
    QPainter *mPainter;
    PainterItem *mItem;
    QColor mPenColor;
    enum { SHAPE_PEN, SHAPE_RECT } mShapeType;
};

#endif // PAINTERWIDGET_H
