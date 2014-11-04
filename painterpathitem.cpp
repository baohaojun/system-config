#include <QGraphicsPathItem>
#include <QGraphicsScene>
#include "painterpathitem.h"

PainterPathItem::PainterPathItem(QObject *parent)
    : PainterItem(parent)
{

}

void PainterPathItem::init(QGraphicsScene *scene, const QPen &pen)
{
    mItem = new QGraphicsPathItem;
    mItem->setPen(pen);
    scene->addItem(mItem);
}

void PainterPathItem::setBeginPoint(const QPointF &pt)
{
    mPainterPath.moveTo(pt);
}

void PainterPathItem::setEndPoint(const QPointF &pt)
{
    mPainterPath.lineTo(pt);
    mItem->setPath(mPainterPath);
}
