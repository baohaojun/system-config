#!/bin/bash

my-beagle "$2"|xargs grep -H -n -I -e "$2"
