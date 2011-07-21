/****************************************************************************************
* Copyright (c) 2011 Patrick von Reth <patrick.vonreth@gmail.com>                      *
*                                                                                      *
* This program is free software; you can redistribute it and/or modify it under        *
* the terms of the GNU General Public License as published by the Free Software        *
* Foundation; either version 2 of the License, or (at your option) any later           *
* version.                                                                             *
*                                                                                      *
* This program is distributed in the hope that it will be useful, but WITHOUT ANY      *
* WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A      *
* PARTICULAR PURPOSE. See the GNU General Public License for more details.             *
*                                                                                      *
* You should have received a copy of the GNU General Public License along with         *
* this program.  If not, see <http://www.gnu.org/licenses/>.                           *
****************************************************************************************/

#include "icon.h"
#include "../snoreserver.h"

#include <QCryptographichash>
#include <QBuffer>
#include <QHash>
#include <QDebug>


QHash<QString,QString> SnoreIcon::hasedImages;

class SnoreIcon::SnoreIconData
{
public:
    SnoreIconData():
        _isLocalFile(false)
    {	}

    SnoreIconData(const QImage &img):
        _img(img),
        _isLocalFile(false)
    {    }

    ~SnoreIconData()
    {	}


    QImage _img;
    QByteArray _data;
    QString _hash;
    bool _isLocalFile;

private:
    SnoreIconData(const SnoreIconData &other)
    {	}


};


SnoreIcon::SnoreIcon()
{
    d = QSharedPointer<SnoreIconData>(new SnoreIconData());
}

SnoreIcon::SnoreIcon(const QImage &img)
{    
    d = QSharedPointer<SnoreIconData>(new SnoreIconData(img));
}

SnoreIcon::SnoreIcon(const SnoreIcon &other):
    d(other.d)
{    }

SnoreIcon::~SnoreIcon()
{    }


const QString &SnoreIcon::hash() const{
    if(d->_hash.isEmpty()){
        QCryptographicHash h(QCryptographicHash::Md5);
        h.addData(imageData());
        d->_hash = h.result().toHex();
    }
    return d->_hash;
}

const QImage &SnoreIcon::image() const{
    return d->_img;
}

const QString &SnoreIcon::localUrl()const{
    QString h = hash();
    if(hasedImages.contains(h))
        return hasedImages[h];
    QString fp = SnoreServer::snoreTMP();
    fp = fp.append("/").append(h).append(".png");
    d->_img.save(fp,"PNG");
    hasedImages[h] = fp;
    return hasedImages[h];
}

const QByteArray &SnoreIcon::imageData() const{
    if(d->_data.isEmpty()){
        QBuffer buffer( &d->_data );
        buffer.open( QBuffer::WriteOnly );
        d->_img.save( &buffer, "PNG" );
    }
    return d->_data;
}

const bool SnoreIcon::isLocalFile() const
{
    return d->_isLocalFile;
}


#include "icon.moc"
