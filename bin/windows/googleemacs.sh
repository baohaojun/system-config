#!/bin/bash

firefox 'http://www.google.com/search?q='"`echo \"$@\"|perl -npe 'chomp; s#([^_0-9a-zA-Z ])#sprintf(\"%%%02x\", ord($1))#seg; s# #+#g'`" &
