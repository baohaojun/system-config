#ifndef PAINTERITEM_H
#define PAINTERITEM_H

#include <QGraphicsScene>
#include <QPen>

class PainterItem : public QObject
{
    Q_OBJECT
public:
    PainterItem(QObject *parent = 0)
        : QObject(parent)
    {

    }

    virtual void init(QGraphicsScene *scene,
                      const QPen &pen = QPen()) = 0;

    virtual void setBeginPoint(const QPointF &pt)
    {
        mBeginPt = pt;
    }

    virtual void setEndPoint(const QPointF &pt) = 0;
protected:
    QPointF mBeginPt;
};

#endif // PAINTERITEM_H
