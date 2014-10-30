#!/bin/bash
my-adb -w reboot bootloader
fastboot wait-for-device
fastboot flash partition dummy_gpt_both0.bin
fastboot erase aboot
fastboot erase sbl1
fastboot erase tz
fastboot erase rpm
fastboot erase sdi
fastboot erase modem
fastboot erase cache
fastboot erase system
fastboot erase recovery
fastboot erase userdata
fastboot erase boot
fastboot reboot

