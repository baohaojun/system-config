#!/bin/bash
for x in "$@"; do echo -n \""$x"\"\ ; done >>~/w3m.log

/usr/bin/w3m "$@"
