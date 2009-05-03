#!/bin/bash
echo "$@" >~/man.txt
set >>~/man.txt
man "$@" 2>&1|tee -a ~/man.txt
which man |tee -a ~/man.txt
man fprintf |tee -a ~/man.txt
