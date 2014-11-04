#include <QtGui>
#include "painterrectitem.h"
#include "painterwidget.h"
#include "painterpathitem.h"

PainterWidget::PainterWidget(QObject *parent) :
    QGraphicsScene(parent), mShapeType(SHAPE_PEN), mPenColor(Qt::red)
{

}

void PainterWidget::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    switch (mShapeType)
    {
    case SHAPE_PEN:
        mItem = new PainterPathItem(this);
        break;
    case SHAPE_RECT:
        mItem = new PainterRectItem(this);
        break;
    default:
        mItem = new PainterPathItem(this);
    }
    QPen pen(mPenColor);
    pen.setWidth(5);
    mItem->init(this, pen);
    mItem->setBeginPoint(event->scenePos());
    QGraphicsScene::mousePressEvent(event);
}

void PainterWidget::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
    if (mItem != NULL) {
       mItem->setEndPoint(event->scenePos());
    }

    QGraphicsScene::mouseMoveEvent(event);
}

void PainterWidget::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
    if (mItem != NULL) {
        mItem->setEndPoint(event->scenePos());
        delete mItem; // not delete scene item
        mItem = NULL;
    }

    QGraphicsScene::mouseReleaseEvent(event);
}

void PainterWidget::usingPen()
{
    mShapeType = SHAPE_PEN;
}

void PainterWidget::usingRect()
{
    mShapeType = SHAPE_RECT;
}

void PainterWidget::setColor(QColor color)
{
    mPenColor = color;
}
