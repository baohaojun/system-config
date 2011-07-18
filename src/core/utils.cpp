#include "utils.h"

Utils::Utils()
{
}


QString Utils::notificationToSNTPString ( Notification notification )
{
    QString out ( "type=SNP#?version=1.1" );
    if ( notification.hintExists ( "SNaction" ) )
        out+=QString ( "#?action="+notification.hint ( "SNaction" ).value<QString>() );
    if ( !notification.application().isEmpty() )
        out+=QString ( "#?app="+notification.application() );
    if ( !notification.alert().isEmpty() )
        out+=QString ( "#?class="+notification.alert() );
    if ( notification.hintExists ( "SnarlIcon" ) )
        out+=QString ( "#?icon="+notification.hint ( "SnarlIcon" ).value<QString>() );
    out+=QString ( "#?title="+notification.title() +"#?text="+notification.text() +"#?timeout="+QString::number ( notification.timeout() ) );
    return out;
}
