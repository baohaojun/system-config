#!/usr/bin/python
import sys
import re
import os
import commands

sohead = re.compile('(.+\.so):')
funchead = re.compile('([0-9a-f]{8}) <(.+)>:')
funcline = re.compile('^[ ]+([0-9a-f]+):.+')
sysdir = "";

def parsestack( lines ):
    crashline = re.compile('.+#[0-9]{2}.+pc.([0-9a-f]{8})  (.+)')
    ret = []
    for l in lines:
        m = crashline.match(l)
        if m:
            addr =  m.groups()[0]
            libname = m.groups()[1]
            cmds = "arm-eabi-addr2line -C -f -e %s%s %s|head -1" % (sysdir, libname, addr)
            print "%s %s" % (l, commands.getoutput(cmds))
    return ret

def parseasm( lines ):
    ret = []
    current = None
    for l in lines:
        m = funchead.match(l)
        if m:
            if current:
                ret.append(current)
            startaddr, funcname =  m.groups()
            current = [ funcname, int(startaddr,16), int(startaddr,16) ]
        m = funcline.match(l)
        if m:
            addr =  m.groups()[0]
            if current != None:
                current[2] = int(addr,16)
        m = sohead.match(l)
        if m:
            so =  m.groups()[0]
            so = os.path.split(so)[1]
    return so, ret

if __name__=="__main__":
    stack, sysdir = sys.argv[1],sys.argv[2]

    stack = parsestack( file(stack).read().split('\n') )

