#!/bin/bash
test $(ls ~/Maildir/*/new/|wc -l) != 0
