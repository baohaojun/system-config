#!/bin/bash

sudo ifconfig eth0:0 192.168.33.89

ssh r66 sudo ifconfig eth0:0 192.168.33.66
ssh r66 nat-vbox eth0 eth0 
