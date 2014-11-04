#ifndef PAINTERRECTITEM_H
#define PAINTERRECTITEM_H

#include <QGraphicsRectItem>
#include <QPointF>
#include "painteritem.h"

class PainterRectItem : public PainterItem
{
    Q_OBJECT
public:
    explicit PainterRectItem(QObject *parent = 0);
    
    void init(QGraphicsScene *scene, const QPen &pen = QPen());
    void setEndPoint(const QPointF &pt);
private:
    QGraphicsRectItem *mItem;
};

#endif // PAINTERRECTITEM_H
