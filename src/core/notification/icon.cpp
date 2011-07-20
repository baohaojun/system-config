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


QHash<QString,QString> NotificationIcon::hasedImages;


NotificationIcon::NotificationIcon():
_data(NULL)
{
}

NotificationIcon::NotificationIcon(const QImage &img):
_img(img)
{    
    QBuffer buffer( _data );
    buffer.open( QBuffer::WriteOnly );
    img.save( &buffer, "PNG" );
	computeHash();
}

NotificationIcon::NotificationIcon(const QByteArray &img)
{
	computeHash();
}

void NotificationIcon::computeHash(){
	if(_hash.isEmpty()){
        QCryptographicHash h(QCryptographicHash::Md5);
		h.addData(*_data);
        _hash = h.result().toHex();
    }
}

const QImage &NotificationIcon::image() const{
	return _img;
}

const QString &NotificationIcon::localUrl()const{
	if(hasedImages.contains(_hash))
		return hasedImages[_hash];
	QString fp = SnoreServer::snoreTMP();
	fp = fp.append("/").append(_hash).append(".png");
	_img.save(fp,"PNG");
	hasedImages[_hash] = fp;
	return hasedImages[_hash];
}



#include "icon.moc"