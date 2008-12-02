#!/bin/bash
for x in "$@"; do grep exec "$x"|grep -v '!'|sed -e 's/.*exec //g' |tee "$x".rc; done
