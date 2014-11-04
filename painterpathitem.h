#ifndef PAINTERPATHITEM_H
#define PAINTERPATHITEM_H

#include <QPainterPath>
#include "painteritem.h"

class QGraphicsPathItem;

class PainterPathItem : public PainterItem
{
    Q_OBJECT
public:
    PainterPathItem(QObject *parent = 0);

    void init(QGraphicsScene *scene, const QPen &pen);
    void setBeginPoint(const QPointF &pt);
    void setEndPoint(const QPointF &pt);
private:
    QGraphicsPathItem *mItem;
    QPainterPath mPainterPath;
};

#endif // PAINTERPATHITEM_H
