#!/bin/bash
cd '/c/Program Files/Borqs/FireBolt'
./ntwtptp.exe -P USB -t MHLV_NTDKB_TIM.bin -f MHLV_NTDKB_h.bin
sleep 3

./WtpDownload.exe -P USB -F dataio_BSTD.mff.fbf
