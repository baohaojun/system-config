#!/bin/bash

my-beagle "$@"|xargs grep -H -n -I "$@"
