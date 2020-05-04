#!/usr/bin/env bash
StartOrStop=${1:-start}
net $StartOrStop  AntiVirService
