#include "painterrectitem.h"
#include <algorithm>
#include <QGraphicsScene>

PainterRectItem::PainterRectItem(QObject *parent)
    : PainterItem(parent)
{

}

void PainterRectItem::init(QGraphicsScene *scene, const QPen &pen)
{
    mItem = new QGraphicsRectItem;
    mItem->setPen(pen);
    scene->addItem(mItem);
}

void PainterRectItem::setEndPoint(const QPointF &pt)
{
    int x1 = std::min(mBeginPt.x(), pt.x());
    int y1 = std::min(mBeginPt.y(), pt.y());
    int x2 = std::max(mBeginPt.x(), pt.x());
    int y2 = std::max(mBeginPt.y(), pt.y());
    mItem->setRect(x1, y1, x2 - x1, y2 - y1);
}
