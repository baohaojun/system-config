#!/bin/python
import sys

file1 = open(sys.argv[1])
file2 = open(sys.argv[2])

blockSize=4096*0x100
y=0
while True:
    contentFile1 = file1.read(blockSize)
    contentFile2 = file2.read(blockSize)

    if not all((len(contentFile1), len(contentFile2))):
        print '%s has %d remaining' % (sys.argv[1], len(contentFile1))
        print '%s has %d remaining' % (sys.argv[2], len(contentFile2))
        break
    for x in range(len(contentFile1)):
        if contentFile1[x]!=contentFile2[x]:
            print 'diff on pos %d 1:%s 2:%s' % (y*blockSize+x, `contentFile1[x]`, `contentFile2[x]`)

    y+=1
            
