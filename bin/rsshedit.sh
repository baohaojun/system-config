#!/bin/bash
(echo -n redit\ ; for x in "$@"; do echo -n \""$x"\"\ ; done)|nc localhost 3456
