#!/bin/bash

cd /d/knowledge/jdk-6u10-docs/
grep '">.*'"$1"'.*<B>' .jdk6.html|head -n 100 > 1.html

