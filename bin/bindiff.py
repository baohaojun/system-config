#!/usr/bin/env python
import sys
from optparse import OptionParser

usage = "Usage: python %s [--seek1/-1 seek_in_file1] [--seek2/-2 seek_in_file2] file1 file2" % sys.argv[0]
parser = OptionParser(usage=usage)
parser.add_option("-1", "--seek1", dest="seek1", default=0, type="int", help="Seek how many bytes in file 1 before start comparing.")
parser.add_option("-2", "--seek2", dest="seek2", default=0, type="int", help="Seek how many bytes in file 2 before start comparing.")

(options, args) = parser.parse_args(args=sys.argv[1:])

if len(args) != 2:
    print "Error: see usage"

file1 = open(args[0])
file2 = open(args[1])

file1.seek(options.seek1)
file2.seek(options.seek2)

blockSize=4096
y=0
while True:
    contentFile1 = file1.read(blockSize)
    contentFile2 = file2.read(blockSize)

    if not all((len(contentFile1), len(contentFile2))):
        if len(contentFile1):
            print '%s has %d (or more) remaining' % (args[0], len(contentFile1))
        if len(contentFile2):
            print '%s has %d (or more) remaining' % (args[1], len(contentFile2))
        break

    if contentFile1 != contentFile2:
        for x in range(min(len(contentFile1), len(contentFile2))):
            if contentFile1[x]!=contentFile2[x]:
                print 'diff on pos %08x:%08x 1:%s 2:%s' % (
                    y*blockSize+x + options.seek1,
                    y*blockSize+x + options.seek2, `contentFile1[x]`, `contentFile2[x]`)

    if len(contentFile1) != len(contentFile2):
        print 'size mismatch: %d:%d' % (len(contentFile1), len(contentFile2))

    y+=1
