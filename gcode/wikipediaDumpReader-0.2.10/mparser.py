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
import time

g_old_time = time.time()
g_cur_time = g_old_time
g_cur_numarticles = 0
g_old_numarticles = 0


def buildIndex(inputbz2Archive):
    f = bz2.BZ2File(inputbz2Archive)
    if f.readline()[0:10] != "<mediawiki":
        sys.stderr.write("Maybe not a correct mediawiki bz2 dump\n");
    blocknum = -1
    global g_cur_numarticles, g_old_numarticles, g_cur_time, g_old_time
    g_cur_numarticles = 0
    maxI = (1 << (struct.calcsize('I')*8))
    def writeBlock(bzblocks):
        global g_cur_numarticles, g_old_numarticles, g_cur_time, g_old_time
        g_cur_time = time.time()
        if (g_cur_time - g_old_time > 10):
            sys.stderr.write("finished %d articles in %d seconds, rate is %.2f per sec\n" % ( g_cur_numarticles - g_old_numarticles, 
                                                                                              g_cur_time - g_old_time,
                                                                                              (g_cur_numarticles - g_old_numarticles)/(g_cur_time - g_old_time)))
            g_old_time = g_cur_time
            g_old_numarticles = g_cur_numarticles
                            
        return bzblocks[0]

    try:
        while 1:
            while 1: # find page start
                l = f.readline()
                start_block = f.tellbzblock()
                if start_block[0] != blocknum:
                    blocknum = writeBlock(start_block)
                if l == "  <page>\n":
                    start = f.tell()
                    break
                if l == "":
                    raise StopIteration

            while 1: # find title and page end
                 l = f.readline()
                 end_block = f.tellbzblock()
                 if end_block[0] != blocknum:
                         blocknum = writeBlock(end_block)
                 if l[0:11] == '    <title>':
                         titleline = l
                 if l == "  </page>\n":
                         ending = f.tell()
                         break
                 if l == "":
                         raise StopIteration

            D = parseString(titleline)
            n = D.getElementsByTagName('title')
            title = n[0].firstChild.nodeValue
            g_cur_numarticles += 1
            sys.stdout.write(("0x%08x " * 9 + "%s\n") % (
                    start_block[0], start_block[2], start_block[3],
                    end_block[0], end_block[2], end_block[3],
                    start_block[0], start - (start_block[1][0] + start_block[1][1] * maxI), ending - start,
                    title.encode('utf-8')))
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

