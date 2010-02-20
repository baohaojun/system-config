#include "interface.h"
#include "snoreserver.h"


void SnorePlugin::setSnore(SnoreServer *snore){
    this->snore=snore;
}

SnoreServer* SnorePlugin::getSnore(){
    return snore.data();
}


#include "interface.moc"
