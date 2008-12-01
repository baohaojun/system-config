#!/bin/bash

getsudo.ep

sudo mount --bind / /mnt/mnt; 
for x in `cat ~/*.list`; do sudo mount --bind $x /mnt/$x; done

sudo chroot /view/`cleartool pwv -set -s`/mnt su - a22242
#sudo chroot /mnt su - a22242

getsudo.ep
for x in `cat ~/*.list`; do sudo umount /mnt/$x; done
sudo umount /mnt/mnt
