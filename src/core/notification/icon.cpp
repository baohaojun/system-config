/****************************************************************************************
 * Copyright (c) 2011-2012 Patrick von Reth <patrick.vonreth@gmail.com>                 *
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

#include <QCryptographicHash>
#include <QBuffer>
#include <QHash>
#include <QFile>
#include <QDebug>
namespace Snore{

class SnoreIcon::SnoreIconData : public QObject
{
    Q_OBJECT
public:
    SnoreIconData():
        _isLocalFile(false)
    {	}

    SnoreIconData(const QImage &img):
        _img(img),
        _isLocalFile(false)
    {}

    SnoreIconData(const QString &url){
        qDebug()<<"Creating SnoreIcon"<<url;
        if(url.startsWith(":/")){
            _img = QImage(url);
            _isLocalFile = false;
        }else if(QFile(url).exists()){
            _isLocalFile = true;
            _localFileName = url;
        }
    }

    ~SnoreIconData()
    {}


    QImage _img;
    QByteArray _data;
    QString _localFileName;
    QString _hash;
    bool _isLocalFile;

};


QHash<QString,QString> SnoreIcon::hasedImages;

SnoreIcon::SnoreIcon()
{
    d = QSharedPointer<SnoreIconData>(new SnoreIconData());
}

SnoreIcon::SnoreIcon(const QImage &img)
{
    d = QSharedPointer<SnoreIconData>(new SnoreIconData(img));
}

SnoreIcon::SnoreIcon(const QString &url){
    d = QSharedPointer<SnoreIconData>(new SnoreIconData(url));
}

SnoreIcon::SnoreIcon(const SnoreIcon &other):
    d(other.d)
{    }

SnoreIcon::~SnoreIcon()
{
    d.clear();
}


const QImage &SnoreIcon::image() const{
    if(d->_img.isNull() && d->_isLocalFile){
        d->_img = QImage(d->_localFileName);
    }
    return d->_img;
}

const QString &SnoreIcon::localUrl()const{
    if(d->_localFileName.isEmpty()){
        if(hasedImages.contains(hash())){
            d->_localFileName =  hasedImages[hash()];
        }else{
            d->_localFileName = SnoreServer::snoreTMP();
            d->_localFileName  = d->_localFileName .append("/").append(hash()).append(".png");
            hasedImages[hash()] = d->_localFileName;
            d->_img.save(d->_localFileName ,"PNG");
        }
    }
    return d->_localFileName;
}

const QByteArray &SnoreIcon::imageData() const{
    if(d->_data.isEmpty()){
        QBuffer buffer( &d->_data );
        buffer.open( QBuffer::WriteOnly );
        d->_img.save( &buffer, "PNG" );
    }
    return d->_data;
}

const QString &SnoreIcon::hash() const{
    if(d->_hash.isEmpty()){
        QCryptographicHash h(QCryptographicHash::Md5);
        h.addData(imageData());
        d->_hash = h.result().toHex();
    }
    return d->_hash;
}

bool SnoreIcon::isLocalFile() const
{
    return d->_isLocalFile;
}

bool SnoreIcon::isEmpty() const{
    return d->_hash.isEmpty() && d->_img.isNull() && d->_localFileName.isEmpty();
}

}

#include "icon.moc"
