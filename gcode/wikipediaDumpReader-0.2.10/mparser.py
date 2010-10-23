try:
	import mod.bz2 as bz2
except ImportError:
	import mod64.bz2 as bz2
import gzip
import sys
import codecs
import struct
from xml.dom.minidom import parseString

def buildIndex(inputbz2Archive, outidxname, outblockname, callback = None):
 # 1. gzip force ascii handlers, dunno how to fix, 2. gzip.open refuses utf8 names
 zindexfile = gzip.GzipFile(fileobj=open(outidxname, 'w'), filename="")
 blocksfile = open(outblockname, 'w')
 f = bz2.BZ2File(inputbz2Archive)
 #print f.tellbzblock()
 #f.readline()
 #print f.tellbzblock()
 if f.readline()[0:10] != "<mediawiki":
	 print "Maybe not a correct mediawiki bz2 dump"
 blocknum = -1
 numarticles = 0
 maxL = (1 << (struct.calcsize('l')*8))
 def writeBlock(bzblocks):
	print "Processing block ", bzblocks[0]
	#print " bzblocks", bzblocks
	if callback:
		callback(bzblocks[2] / 1024 / 1024)
	blocknum = bzblocks[0]
	blocksfile.write("%d\t%d %d\n" % (blocknum, bzblocks[2], bzblocks[3]))
	return blocknum

 try:
  while 1:
   while 1:
        l = f.readline()
	bzblocks1 = f.tellbzblock()
	if bzblocks1[0] != blocknum:
		blocknum = writeBlock(bzblocks1)
		print numarticles, " articles found"
        if l == "  <page>\n":
		start = f.tell()
                break
	if l == "":
		raise StopIteration

   #print "article found at (unziped) offset : ", start

   while 1:
        l = f.readline()
	bzblocks2 = f.tellbzblock()
	if bzblocks2[0] != blocknum:
		blocknum = writeBlock(bzblocks2)
		print numarticles, " articles found"
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
   numarticles += 1

   zindexfile.write( ("%s\t%s\t%d\t%d\n" % (title, bzblocks1[0], start - (bzblocks1[1][0] + bzblocks1[1][1] * maxL), ending - start) ).encode('utf-8') )
 except StopIteration:
   print "End of that block : ", `f.name`

if __name__ == '__main__':
	filename = sys.argv[1]
	if filename.endswith('.xml.bz2'):
		outidxname = filename[:-8] + '.idx.gz'
		outblockname = filename[:-8] + '.blocks.idx'
		buildIndex(sys.argv[1], outidxname, outblockname)
	else:
		print "first argument should be a wikipedia .xml.bz2 file"
