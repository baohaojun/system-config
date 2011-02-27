#!/bin/bash

firefox 'http://localhost:8000/keyword/'"`echo \"$@\"|perl -npe 'chomp; s#([^_0-9a-zA-Z ])#sprintf(\"%%%02x\", ord($1))#seg; '`" &
