#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os, sys
import re
import codecs
from xml.dom.minidom import parseString
from xml.parsers.expat import ExpatError
try:
	import mod.bz2 as bz2
except ImportError:
	import mod64.bz2 as bz2
import random

def die(arg):
    print arg
    sys.exit(-1)

f = bz2.BZ2File("/home/bhj/wikipedia/enwiki-latest-pages-articles.xml.bz2")
prev_block_num, prev_block_byte, prev_block_bit, next_block_num, next_block_byte, next_block_bit, block, start, len_ = map(int, sys.argv[1:])

if block == prev_block_num:
    block_byte = prev_block_byte
    block_bit = prev_block_bit
elif block == next_block_num:
    block_byte = next_block_byte
    block_bit = next_block_bit
else:
    die("Error: current block is not one of prev/next block")

L, olength = f.loadBlock(block_byte, block_bit, start, len_)
if olength - start < len_:
    L2, _ = f.loadBlock(next_block_byte, next_block_bit, 0, len_-(olength - start))
    L  = L[:olength-start] + L2[:start + len_-olength]


try:
    D = parseString("<page>\n" + L)
except ExpatError:
    die("Error : can't load this article - sorry")

n = D.getElementsByTagName('title')
title = n[0].firstChild.nodeValue

n = D.getElementsByTagName('text')
text = n[0].firstChild.nodeValue
sys.stdout.write("%s\n%s" % (title.encode('utf-8'), text.encode('utf-8')))
