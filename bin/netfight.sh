#!/bin/bash
netsh interface ip set address name=eth0 source=static addr=$1 mask=255.255.255.0 gateway=$1 gwmetric=1
netsh interface ip set dns name=eth0 source=static addr=202.109.122.14 register=primary
netsh interface ip add dns name=eth0 addr=210.52.149.2 index=2
