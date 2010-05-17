#!/usr/bin/env python
import sys
n=eval(sys.argv[1])

if n>>64 != 0:
    print "Error: descriptor value too big!"
    sys.exit(0)

base = (n >> 56 << 24)|((n >> 16)&((1 << 24) - 1))
print 'base is 0x%x' % base

limit = (n & ((1<<16) - 1)) | (((n>>48) & ((1<<4)-1))<<16)
print 'limit is 0x%x' % limit

G = (n&(1<<55))>>55
print 'G is %d (1 means big grain (max 4G), 0 means small grain (max 1M))' % G

D = (n&(1<<54))>>54
print 'D is %d (1 means 32 bit offset, 0 means 16 bit offset)' % D

AVL = (n&(1<<52))>>52
print 'AVL is %d (this bit is available for use by system software)' % AVL

high = n>>32

P = (high & (1<<15)) >>15
print 'P is %d (1 means present)' % P

DPL = (high>>13) & 0x3
print 'DPL (descriptor priviledge level) is %d' % DPL

S = (high>>12)&0x1
print 'U/S is %d (1 means user code/data, 0 means system)' % S

Type = (high>>8) & 0xf

TypeStr = (('Reserved',
'16-Bit TSS (Available)',
'LDT',
'16-Bit TSS (Busy)',
'16-Bit Call Gate',
'Task Gate',
'16-Bit Interrupt Gate',
'16-Bit Trap Gate',
'Reserved',
'32-Bit TSS (Available)',
'Reserved',
'32-Bit TSS (Busy)',
'32-Bit Call Gate',
'Reserved',
'32-Bit Interrupt Gate',
'32-Bit Trap Gate',
), 
('Read-Only',
'Read-Only, accessed',
'Read/Write',
'Read/Write, accessed',
'Read-Only, expand-down',
'Read-Only, expand-down, accessed',
'Read/Write, expand-down',
'Read/Write, expand-down, accessed',
'Execute-Only',
'Execute-Only, accessed',
'Execute/Read',
'Execute/Read, accessed',
'Execute-Only, conforming',
'Execute-Only, conforming, accessed',
'Execute/Read-Only, conforming',
'Execute/Read-Only, conforming, accessed',
))

print 'Type is 0x%x (%s)' % (Type, TypeStr[S][Type])
