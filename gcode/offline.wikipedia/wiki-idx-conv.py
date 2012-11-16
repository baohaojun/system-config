#!/usr/bin/env python
import re, struct, sys
from xml.dom.minidom import parseString

re_title = re.compile("^title: (.*)" + "\t(-?0x[0-9a-fA-F]*)"*3 + "$")
re_block = re.compile("^block: " + "(-?0x[0-9a-fA-F]*)\t?"*3 + "$")

prev_block = cur_block = None

def Int(x):
    return int(x, 0)

while 1:
    l = sys.stdin.readline()
    if l == "":
        break

    bmatch = re_block.match(l)
    if bmatch:
        prev_block = cur_block
        cur_block = map(Int, (bmatch.group(1), bmatch.group(2), bmatch.group(3)))
        if not prev_block:
            prev_block = cur_block
    else:
        tmatch = re_title.match(l)
        if tmatch:
            t = "<title>" + tmatch.group(1) + "</title>"
            D = parseString(t)
            n = D.getElementsByTagName('title')
            title = n[0].firstChild.nodeValue.encode('utf-8')

            start = Int(tmatch.group(3))
            if (start < 0):
                # this is work-around for a bug where wikidump-reader incorrectly used 64bit instead of 32bit for the hi-4byte calc.
                I_bits = struct.calcsize('I') * 8
                l_bits = struct.calcsize('l') * 8
                maxI = 1 << I_bits
                maxl = 1 << l_bits

                bh = -start/maxl + 1 
                start = start + bh*maxl - bh*maxI


            sys.stdout.write (("0x%08x " * 9 + "%s\n") % (
                prev_block[0], prev_block[1], prev_block[2],
                cur_block[0], cur_block[1], cur_block[2],
                Int(tmatch.group(2)), start, Int(tmatch.group(4)),
                title))
            prev_block = cur_block
