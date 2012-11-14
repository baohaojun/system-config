#!/bin/bash

for x in *.human; do
    marvell-human-to-partition $x > ${x%.human}
done

./mbrgen partition_4g 0xe9000000 
mv primary_gpt_3909091328 primary_gpt_4g
mv second_gpt_3909091328 secondary_gpt_4g

./mbrgen partition 0x3aa000000
mv primary_gpt_15737028608 primary_gpt
mv second_gpt_15737028608 secondary_gpt
