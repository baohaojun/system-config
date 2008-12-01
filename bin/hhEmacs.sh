#!/bin/bash
(
    echo ImeSelect;
    while true; do
        nc -l -p 3837 </dev/null
    done
    ) | ~/bin/dexplore.exe&
