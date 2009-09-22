#!/bin/bash
StartOrStop=${1:-start}
net $StartOrStop  AntiVirService
