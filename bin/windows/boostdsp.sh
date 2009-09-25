#!/bin/bash

perl -npe 's! /FD ! !g; s! /YX ! !g; s! /Y[cu]".*?" !!g; ' -i "$@"

