#!/usr/bin/env python
try:
	import mod.bz2 as bz2
except ImportError:
	import mod64.bz2 as bz2
import gzip
import sys
import codecs
import struct
from xml.dom.minidom import parseString

def buildIndex(inputbz2Archive):
    f = bz2.BZ2File(inputbz2Archive)
    if f.readline()[0:10] != "<mediawiki":
        sys.stderr.write("Maybe not a correct mediawiki bz2 dump\n");
    blocknum = -1
    numarticles = 0
    maxI = (1 << (struct.calcsize('I')*8))
    def writeBlock(bzblocks):
        sys.stderr.write("Processing block 0x%x\n" % bzblocks[0]);
        sys.stdout.write("block: 0x%x\t0x%x\t0x%x\n" % (bzblocks[0], bzblocks[2], bzblocks[3]))
        return bzblocks[0]

    try:
        while 1:
            while 1: # find page start
                l = f.readline()
                bzblocks1 = f.tellbzblock()
                if bzblocks1[0] != blocknum:
                    blocknum = writeBlock(bzblocks1)
                    sys.stderr.write("0x%x articles found\n" % numarticles)
                if l == "  <page>\n":
                    start = f.tell()
                    break
                if l == "":
                    raise StopIteration

            while 1: # find title and page end
                 l = f.readline()
                 bzblocks2 = f.tellbzblock()
                 if bzblocks2[0] != blocknum:
                         blocknum = writeBlock(bzblocks2)
                         sys.stderr.write("0x%x articles found\n" % numarticles)
                 if l[0:11] == '    <title>':
                         titleline = l
                 if l == "  </page>\n":
                         ending = f.tell()
                         break
                 if l == "":
                         raise StopIteration

            title = titleline[11:-9]
            numarticles += 1

            sys.stdout.write(("title: %s\t0x%x\t0x%x\t0x%x\n" % (title, bzblocks1[0], start - (bzblocks1[1][0] + bzblocks1[1][1] * maxI), ending - start) ))
    except StopIteration:
        sys.stderr.write("End of that block : %s\n" % `f.name`)

if __name__ == '__main__':
	filename = sys.argv[1]
	if filename.endswith('.xml.bz2'):
		buildIndex(sys.argv[1]);
	else:
		sys.stderr.write("first argument should be a wikipedia .xml.bz2 file\n")

# Local Variables: #
# tab-width: 4 #
# python-indent: 4 #
# End: #

