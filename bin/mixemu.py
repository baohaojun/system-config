#This is MIX emulator. But let's do a little design first

#1. what is the input?

#The input should be a MIX binary program. Not a MIXAL (MIX assembly language) source program.
#So, I will also need an MIX assembly to generate the binary program from the assembly program.

#2. What is the binary program like? Or, put it another way, what is the goal of this emulator?

#I want to make this emulator look like a true MIX machine as closely as possible. That is, there
#should be a `GO' button, just like the MIX described in Knuth's book. Because the goal is so that 
#I'm able to do most, if not all, of the exercises in his book.

#The more desirable goal is to write a MIX assembler in MIXAL. But that is currently too difficult 
#for me. So, maybe later.

#So, I will write a MIX assembler and a MIX emulator both in python. Great! And the emulator is supposed
#to be easier. Because the assembler will have to deal with AL grammer (parsing, etc.:-), while the 
#emulator will only have to deal with fixed-size machine language.

import os, sys
from ascii2mix import *
def StartMachine ():
    readLoaderCard1To0()
    JmpTo0()

MixByteLimit = 81 #or, it can be 100, 64, 81, etc
_maxByteValue = MixByteLimit-1
_maxWordVal = 0
for x in range(1, 6):
    _maxWordVal = _maxWordVal*MixByteLimit + _maxByteValue
MixWordLimit = _maxWordVal+1

#the x86 representation of MIX word will be an x86 int (or, equivalently for x86, long, 4 x86-bytes..).
#bit 0-5,    bit 6-11, bit 12-17, bit 18-23, bit 24-29, bit 30
#mix byte1, mix byte2, mix byte3, mix byte4, mix byte5, mix sign
def signof(v):
    if v > 0:
        return 1
    elif v < 0:
        return -1
    else:
        return 0

def breakF(f): #x:y will parse to a tuple (x, y+1), instead of (x, y), because the former makes more sense
    f1 = f/8
    f2 = f%8
    assert(f1>=0 and f2>=f1 and f2<=5)
    return f1, f2+1


class MixMachine:
    def __init__(self):
        self.areg = mixFullWord()
        self.xreg = mixFullWord()
        self.jreg = mix2byte()
        self.i1reg = mix2byte()
        self.i2reg = mix2byte()
        self.i3reg = mix2byte()
        self.i4reg = mix2byte()
        self.i5reg = mix2byte()
        self.i6reg = mix2byte()
        self.zreg = mixFullWord() #this should always stay ZERO!

        self.overflow = False
        self.cmpFlag = 0 #-1 for less, 0 for equal, 1 for greater

        self.iregs = [None, #there's no i0
                      self.i1reg, self.i2reg, self.i3reg,
                      self.i4reg, self.i5reg, self.i6reg] 

        self.regs = [self.areg,
                     self.i1reg, self.i2reg, self.i3reg,
                     self.i4reg, self.i5reg, self.i6reg, 
                     self.xreg, self.jreg, self.zreg]

        self.memory = [None]*4000
        for x in range(0, 4000):
            self.memory[x] = mixFullWord()

        self.tapes = [None]*8
        for x in range(0, 8):
            self.tapes[x] = mixTape(x)

        self.disks = [None]*8
        for x in range(0, 8):
            self.disks[x] = mixDisk(x)

        self.cardReader = mixCardReader()
        self.cardPunch = mixCardPunch()
        self.linePrinter = mixLinePrinter()
        self.terminal = mixTerminal()
        self.paperTape = mixPaperTape()

        self.ioDevices = self.tapes + self.disks + [
            self.cardReader,
            self.cardPunch,
            self.linePrinter,
            self.terminal,
            self.paperTape]
            
        

        self.ip = 0 #this is the instruction pointer

        self.operations = {
            0:self.nop,1:self.add,2:self.sub,3:self.mul,4:self.div,
            5:self.special,6:self.shift,7:self.move,8:self.lda,9:self.ld1,
            10:self.ld2,11:self.ld3,12:self.ld4,13:self.ld5,14:self.ld6,
            15:self.ldx,16:self.ldan,17:self.ld1n,18:self.ld2n,19:self.ld3n,
            20:self.ld4n,21:self.ld5n,22:self.ld6n,23:self.ldxn,24:self.sta,
            25:self.st1,26:self.st2,27:self.st3,28:self.st4,29:self.st5,
            30:self.st6,31:self.stx,32:self.stj,33:self.stz,34:self.jbus,
            35:self.ioc,36:self.in_,37:self.out,38:self.jrdy,39:self.jumps,
            40:self.ja,41:self.j1,42:self.j2,43:self.j3,44:self.j4,
            45:self.j5,46:self.j6,47:self.jx,48:self.areg_op,49:self.i1reg_op,
            50:self.i2reg_op,51:self.i3reg_op,52:self.i4reg_op,53:self.i5reg_op,54:self.i6reg_op,
            55:self.xreg_op,56:self.cmpa,57:self.cmp1,58:self.cmp2,59:self.cmp3,
            60:self.cmp4,61:self.cmp5,62:self.cmp6,63:self.cmpx,
            }


    def nop(self, a, i, f, c):
        pass

    def getM(self):
        cWord = self.memory[self.ip]
        (a, i, f, c) = cWord.decode()
        return self._getM(a, i)

    def _getM(self, a, i):
        m = a + self.iregs[i].value
        #assert (m>=0 and m<4000) #this assert is only valid if M refer to a memory cell
        return m

    def _getV(self, a, i, f):
        m = self._getM(a, i)
        v = self.memory[m].getFieldValue(f)
        return v

    def _getS(self, a, i, f):
        m = self._getM(a, i)
        s = self.memory[m].getFieldSign(f)
        return s


    def add(self, a, i, f, c): #
        v = self._getV(a, i, f)
        overflow = self.areg.add(v)        
        if overflow: #otherwise the overflow toggle is unchanged
            self.overflow = overflow
        pass
    def sub(self, a, i, f, c):
        v = self._getV(a, i, f)
        overflow = self.areg.add(-v)
        if overflow:
            self.overflow = overflow
        pass
    def mul(self, a, i, f, c):

        s = self.areg.sign * self._getS(a, i, f)
        v = self._getV(a, i, f)
        res = abs (self.areg.magnitude * v)

        amag = res/MixWordLimit
        xmag = res%MixWordLimit

        self.areg.magnitude = amag
        self.xreg.magnitude = xmag
        self.areg.sign = self.xreg.sign = s        
        
        pass
    def div(self, a, i, f, c):
        signRegX = self.areg.sign
        signRegA = self.areg.sign * self._getS(a, i, f)

        devidee = self.areg.magnitude*MixWordLimit + self.xreg.magnitude

        v = self._getV(a, i, f)
        if v==0 or abs(devidee/v)>_maxWordVal: #random value in rega and regx, overflow is set
            self.areg.formatAssign(signRegA, 1, 2, 3, 4, 5)
            self.xreg.formatAssign(signRegX, 5, 4, 3, 2, 1) 
            self.overflow = True
        else:
            import math
            self.areg.magnitude = int(math.floor(abs(devidee/float(v))))
            self.areg.sign = signRegA
            self.xreg.magnitude = devidee%abs(v)
            self.xreg.sign = signRegX
        pass

    def special(self, a, i, f, c): #NUM(f=0), CHAR(f=1), HLT(f=2)
        if (f == 0):
            self.num(a, i, f, c) #seems a, i, f, c, is not needed here
        elif (f == 1):
            self.char(a, i, f, c) #same, not needed
        elif (f == 2):
            self.hlt(a, i, f, c)
        else:
            raise RuntimeError, 'illegal special f/c combination %d/%d' % (f, c)
        pass

    def num(self, a, i, f, c):
        digits = self.areg[1:6] + self.xreg[1:6]

        mag = 0
        for x in digits:
            mag = mag*10+(x%10)

        if mag > _maxWordVal: #overflow is possible, but he didn't specify if the `overflow' flag is set
            #modula b**5, b is the byte size.
            mag = mag % MixWordLimit
        self.areg.magnitude = mag #sign of RA and value of RX is unaffected
        pass

    def char(self, a, i, f, c):
        mag = self.areg.magnitude
        digits = [30]*10 #30 is the mix code for char '0'
        for x in range(0, 10):
            digits[x] = mag%10+30
            mag = mag/10

        digits.reverse()

        self.areg[1:6] = digits[0:5]
        self.xreg[1:6] = digits[5:10]
        pass
    def hlt(self, a, i, f, c):
        print 'MIX halted, press any key to resume'
        raw_input()
        pass
    def shift(self, a, i, f, c): #sla(0), sra(1), slax(2), srax(3), slc(4), src(5)
        m = self._getM(a, i)
        assert(m>=0) #M must be nonnegative

        if f == 0: #sla
            bytes = (self.areg[1:] + [0]*m)[-5:]
            self.areg[1:] = bytes
        elif f == 1: #sra
            bytes = ([0]*m + self.areg[1:])[0:5]
            self.areg[1:] = bytes
        elif f == 2: #slax
            bytes = (self.areg[1:]+self.xreg[1:]+[0]*m)[-10:]
            self.areg[1:] = bytes[0:5]
            self.xreg[1:] = bytes[5:]
        elif f == 3: #srax
            bytes = ([0]*m + self.areg[1:] + self.xreg[1:])[0:10]
            self.areg[1:] = bytes[0:5]
            self.xreg[1:] = bytes[5:]
        elif f == 4: #slc
            bytes = ((self.areg[1:] + self.xreg[1:])*2)[m%10:m%10+10]
            self.areg[1:] = bytes[0:5]
            self.xreg[1:] = bytes[5:]
        elif f == 5: #src
            bytes = ((self.areg[1:] + self.xreg[1:])*2)[-m%10:-m%10+10]
            self.areg[1:] = bytes[0:5]
            self.xreg[1:] = bytes[5:]
        else:
            raise RuntimeError, 'illegal shift f/c combination %d/%d' % (f, c)            
        
        pass
    def move(self, a, i, f, c):
        m = self._getM(a, i)
        for x in range(0, f):
            self.memory[m+x][:] = self.memory[self.i1reg.value][:]
            self.i1reg.add(1)
        pass
    def lda(self, a, i, f, c):
        self.generalLdr(a, i, f, c)

    def generalLdr(self, a, i, f, c): #which reg can be decided from c
        whichReg = self.regs[c-8] #8 for areg, 9-14 for iregs, 15 for xreg
        v = self._getV(a, i, f)
        m = self._getM(a, i)
        if v != 0:
            whichReg.magnitude = abs(v)
            whichReg.sign = signof(v)
        elif v == 0:
            whichReg.magnitude = 0
            if f < 8: #the sign bit is included in f
                whichReg.sign = self.memory[m].sign
            else:
                whichReg.sign = 1 # a plus is understood
        pass
    def ld1(self, a, i, f, c):
        self.generalLdr(a, i, f, c)
        pass

    def ld2(self, a, i, f, c):
        self.generalLdr(a, i, f, c)
        pass
    def ld3(self, a, i, f, c):
        self.generalLdr(a, i, f, c)
        pass
    def ld4(self, a, i, f, c):
        self.generalLdr(a, i, f, c)
        pass
    def ld5(self, a, i, f, c):
        self.generalLdr(a, i, f, c)
        pass
    def ld6(self, a, i, f, c):
        self.generalLdr(a, i, f, c)
        pass

    def ldx(self, a, i, f, c):
        self.generalLdr(a, i, f, c)
        pass
    def generalLdrN(self, a, i, f, c):
        c = c-16+8 #16 is c for ldan, 8 will be subtracted below
        whichReg = self.regs[c-8] #8 for areg, 9-14 for iregs, 15 for xreg
        self.generalLdr(a, i, f, c)
        whichReg.sign = -whichReg.sign #the opposite sign

    def ldan(self, a, i, f, c):
        self.generalLdrN(a, i, f, c)
        pass
    def ld1n(self, a, i, f, c):
        self.generalLdrN(a, i, f, c)
        pass
    def ld2n(self, a, i, f, c):
        self.generalLdrN(a, i, f, c)
        pass
    def ld3n(self, a, i, f, c):
        self.generalLdrN(a, i, f, c)
        pass

    def ld4n(self, a, i, f, c):
        self.generalLdrN(a, i, f, c)
        pass
    def ld5n(self, a, i, f, c):
        self.generalLdrN(a, i, f, c)
        pass
    def ld6n(self, a, i, f, c):
        self.generalLdrN(a, i, f, c)
        pass
    def ldxn(self, a, i, f, c):
        self.generalLdrN(a, i, f, c)
        pass

    def generalStr(self, a, i, f, c):
        c = c - 24 + 8 #24 is c for sta, 8 is c for lda, and it will be subtracted below
        whichReg = self.regs[c-8] #8 for areg, 9-14 for iregs, 15 for xreg
        f1, f2 = breakF(f)
        m = self._getM(a, i)

        if f1 == 0:
            self.memory[m].sign = whichReg.sign #else the sign of the memory cell is not altered
            f1 = f1+1

        bytes = whichReg[f1-f2:] #f=(2:4) ->3 bytes, f1=2, f2=5 -> whichReg[2-5:] -> 3 bytes
        self.memory[m][f1:f2] = bytes
        
    def sta(self, a, i, f, c):
        self.generalStr(a, i, f, c)
        pass

    def st1(self, a, i, f, c):
        self.generalStr(a, i, f, c)
        pass
    def st2(self, a, i, f, c):
        self.generalStr(a, i, f, c)
        pass
    def st3(self, a, i, f, c):
        self.generalStr(a, i, f, c)
        pass
    def st4(self, a, i, f, c):
        self.generalStr(a, i, f, c)
        pass
    def st5(self, a, i, f, c):
        self.generalStr(a, i, f, c)
        pass

    def st6(self, a, i, f, c):
        self.generalStr(a, i, f, c)
        pass
    def stx(self, a, i, f, c):
        self.generalStr(a, i, f, c)
        pass
    def stj(self, a, i, f, c):
        self.generalStr(a, i, f, c)
        pass
    def stz(self, a, i, f, c):
        self.generalStr(a, i, f, c)
        pass

    def jbus(self, a, i, f, c): #this emulator will never be buzy, so, jump never happen here
        pass

    def ioc(self, a, i, f, c): #F is the unit number
        self.ioDevices[f].ioc(self)
        
        pass
    def in_(self, a, i, f, c):
        self.ioDevices[f].in_(self)
        pass
    def out(self, a, i, f, c):
        self.ioDevices[f].out(self)
        pass

    def doActualJump(self, a, i, f, c): #1. set the PC accordingly, 2. set the RJ accordingly
        self.jreg.value = self.ip+1
        self.ip = self._getM(a, i) - 1 #because in exec_1, the ip will be incremented, so we undo that effect here
        
    def jrdy(self, a, i, f, c): #this jump will always happen
        self.doActualJump(a, i, f, c)
        pass
    def jumps(self, a, i, f, c):
        if f == 0: #unconditional jump
            self.doActualJump(a, i, f, c)
        elif f == 1: #JSJ
            self.ip = self._getM(a, i) - 1
        elif f == 2: #JOV
            if self.overflow:
                self.overflow = False
                self.doActualJump(a, i, f, c)
        elif f == 3: #JNOV
            if self.overflow:
                self.overflow = False
            else:
                self.doActualJump(a, i, f, c)
        elif f == 4: #JL
            if self.cmpFlag == -1:
                self.doActualJump(a, i, f, c)
        elif f == 5: #JE
            if self.cmpFlag == 0:
                self.doActualJump(a, i, f, c)
        elif f == 6: #JG
            if self.cmpFlag == 1:
                self.doActualJump(a, i, f, c)
        elif f == 7: #JGE
            if self.cmpFlag != -1:
                self.doActualJump(a, i, f, c)
        elif f == 8: #JNE
            if self.cmpFlag != 0:
                self.doActualJump(a, i, f, c)
        elif f == 9:
            if self.cmpFlag != 1:
                self.doActualJump(a, i, f, c)
        else:
            raise RuntimeError, 'illegal jump f/c combination %d/%d' % (f, c)
        pass

    def generalJR(self, a, i, f, c): #general Jump according to Register
        c = c-40+8 #40 is for ja*
        whichReg = self.regs[c-8]
        if f == 0: #J?N
            if whichReg.value < 0:
                self.doActualJump(a, i, f, c)
        elif f == 1: #J?Z
            if whichReg.value == 0:
                self.doActualJump(a, i, f, c)
        elif f == 2: #J?P
            if whichReg.value > 0:
                self.doActualJump(a, i, f, c)
        elif f == 3: #J?NN
            if not whichReg.value < 0:
                self.doActualJump(a, i, f, c)
        elif f == 4: #J?NZ
            if not whichReg.value == 0:
                self.doActualJump(a, i, f, c)
        elif f == 5: #J?NP
            if not whichReg.value > 0:
                self.doActualJump(a, i, f, c)
        else:
            raise RuntimeError, 'illegal JR f/c combination %d/%d' % (f, c)
    def ja(self, a, i, f, c):
        self.generalJR(a, i, f, c)
        pass
    def j1(self, a, i, f, c):
        self.generalJR(a, i, f, c)
        pass
    def j2(self, a, i, f, c):
        self.generalJR(a, i, f, c)
        pass
    def j3(self, a, i, f, c):
        self.generalJR(a, i, f, c)
        pass
    def j4(self, a, i, f, c):
        self.generalJR(a, i, f, c)
        pass

    def j5(self, a, i, f, c):
        self.generalJR(a, i, f, c)
        pass
    def j6(self, a, i, f, c):
        self.generalJR(a, i, f, c)
        pass
    def jx(self, a, i, f, c):
        self.generalJR(a, i, f, c)
        pass

    def generalRegOp(self, a, i, f, c, whichReg):
        if (f==0): #INCR
            M = self._getM(a, i)
            overflow = whichReg.add(M)
            if overflow:
                self.overflow = overflow
            else:
                pass  #"Otherwise the overflow toggle is unchanged"
        elif (f==1): #DECR
            M = self._getM(a, i)
            overflow = whichReg.add(-M)
            if overflow:
                self.overflow = overflow
            else:
                pass

        elif (f==2): #ENTR
            #here we must pay attention to sign of the instruction word
            iWord = self.memory[self.ip]
            iWordCopy = mixFullWord()
            
            iWordCopy.sign = iWord.sign
            iWordCopy[4:6] = iWord[1:3]
            iWordCopy.add(self.iregs[i].value)
            whichReg.sign = iWordCopy.sign
            whichReg.magnitude = iWordCopy.magnitude
        elif (f==3): #ENTNR
            iWord = self.memory[self.ip]
            iWordCopy = mixFullWord()

            iWordCopy.sign = iWord.sign
            iWordCopy[4:6] = iWord[1:3]
            iWordCopy.add(self.iregs[i].value)
            whichReg.sign = -iWordCopy.sign
            whichReg.magnitude = iWordCopy.magnitude
        else:
            raise RuntimeError, 'illegal  f/c combination %d/%d' % (f, c)
        pass

    def areg_op(self, a, i, f, c): #inc(f=0), dec(f=1), ent(f=2), enn(f=3)
        self.generalRegOp(a, i, f, c, self.areg)

    def i1reg_op(self, a, i, f, c):
        self.generalRegOp(a, i, f, c, self.i1reg)
        pass

    def i2reg_op(self, a, i, f, c):
        self.generalRegOp(a, i, f, c, self.i2reg)
        pass
    def i3reg_op(self, a, i, f, c):
        self.generalRegOp(a, i, f, c, self.i3reg)
        pass
    def i4reg_op(self, a, i, f, c):
        self.generalRegOp(a, i, f, c, self.i4reg)
        pass
    def i5reg_op(self, a, i, f, c):
        self.generalRegOp(a, i, f, c, self.i5reg)
        pass
    def i6reg_op(self, a, i, f, c):
        self.generalRegOp(a, i, f, c, self.i6reg)
        pass
    def xreg_op(self, a, i, f, c):
        self.generalRegOp(a, i, f, c, self.xreg)
        pass

    def generalCmpR(self, a, i, f, c): #general cmp R
        c = c-56+8 # 56 for `C' of cmpA
        whichReg = self.regs[c-8]

        rv = whichReg.getFieldValue(f)
        mv = self._getV(a, i, f)

        if rv < mv:
            self.cmpFlag = -1
        elif rv == mv:
            self.cmpFlag = 0
        elif rv > mv:
            self.cmpFlag = 1
        
    def cmpa(self, a, i, f, c):
        self.generalCmpR(a, i, f, c)
        pass
    def cmp1(self, a, i, f, c):
        self.generalCmpR(a, i, f, c)
        pass
    def cmp2(self, a, i, f, c):
        self.generalCmpR(a, i, f, c)
        pass
    def cmp3(self, a, i, f, c):
        self.generalCmpR(a, i, f, c)
        pass

    def cmp4(self, a, i, f, c):
        self.generalCmpR(a, i, f, c)
        pass
    def cmp5(self, a, i, f, c):
        self.generalCmpR(a, i, f, c)
        pass
    def cmp6(self, a, i, f, c):
        self.generalCmpR(a, i, f, c)
        pass
    def cmpx(self, a, i, f, c):
        self.generalCmpR(a, i, f, c)
        pass

    def exec_1(self, ip=-1):
        """ip=-1: the self.ip is used, or else set self.ip to ip first"""

        if ip != -1:
            self.ip = ip
        cWord = self.memory[self.ip]
        (a, i, f, c) = cWord.decode()
        self.operations[c](a, i, f, c)
        self.ip = self.ip + 1

def formatForAssign(*args): #for, e.g., args=((i, n), i2), then the int `i' is broken into `n' mix bytes, followed by 1 byte of `i2'
    bytes = [0]*6
    currentN = 0
    for x in args:
        if type(x) in (long, int):
            bytes[currentN] = x
            currentN = currentN+1
        elif type(x) in (tuple, list):
            assert(len(x) == 2)
            i = x[0]
            n = x[1]
            for y in range(currentN+n-1, currentN-1, -1):
                bytes[y] = i%MixByteLimit
                i = i/MixByteLimit
            currentN = currentN+n

    assert(currentN <= 6)
    return bytes[0:currentN]
        
def makeInt(seq):
    v = 0
    for x in seq:
        v = v*MixByteLimit + x
    return v

class mixFullWord(object):
    def __init__(self, sign=1, b1=0, b2=0, b3=0, b4=0, b5=0):
        self.bytes = []
        self.bytes[0:6] = [sign, b1, b2, b3, b4, b5]
        
    def __getitem__(self, key):
        return self.bytes[key]

    def __setitem__(self, key, value):
        self.bytes[key] = value
        if len(self.bytes)!=6:
            raise ValueError, 'after assignment, mixFullWord become %s' % `self`
        if self.bytes[0] not in (-1, 1):
            raise ValueError, 'after assignment, mixFullWord sign illegal: %s' % `self`


    def decode(self):
        a = (self.bytes[1]*MixByteLimit + self.bytes[2])*self.sign
        i = self.bytes[3]
        f = self.bytes[4]
        c = self.bytes[5]
        return (a, i, f, c)

    def getSign(self):
        return self.bytes[0]

    def setSign(self, s):
        if s == +1:
            self.bytes[0] = 1
        elif s == -1:
            self.bytes[0] = -1
        else:
            raise RuntimeError, 'illegal sign %s' % str(s)

    sign = property(getSign, setSign)

    def getMagnitude(self):
        mag = 0
        for x in range(1, 6):
            mag = mag*MixByteLimit + self.bytes[x]

        return mag

    def setMagnitude(self, mag):
        if mag < 0:
            mag = -mag

        for x in range(5, 0, -1):
            self.bytes[x] = int(mag%MixByteLimit) #in case the result is a `long', cast it to `int'
            mag = mag/MixByteLimit

    magnitude = property(getMagnitude, setMagnitude)

    def getValue(self):
        return self.magnitude*self.sign

    #There should not be setValue, because the sign is unclear when abs(v)=0, 
    #In this case, we just stick to the magnitude attribute
    # def setValue(self, v):
    #     self.sign = v

    value = property(getValue)

    def getFieldValue(self, f):
        f1, f2 = breakF(f)
        if (f1==0):
            sign = self.sign
            f1 = f1+1
        else:
            sign = 1
            
        v = 0
        for x in range(f1, f2):
            v = v*MixByteLimit + self.bytes[x]

        return sign*v

    def getFieldSign(self, f):
        f1 = f/8
        f2 = f%8
        assert(f1>=0 and f2>=f1 and f2<=5)

        if (f1==0):
            sign = self.sign
        else:
            sign = 1

        return sign


    def add(self, v):
        overflow = False 
        res = v+self.value

        if res == 0:
            sign = self.sign #sign must not change
        else:
            sign = 1 if res > 0 else -1
        if res < -_maxWordVal:
            res += MixWordLimit
            overflow = True
        elif res > _maxWordVal:
            res -= MixWordLimit
            overflow = True

        self.magnitude = abs(res)
        self.sign = sign
        return overflow

    def __repr__(self):
        return `self.bytes`

    def formatForPrint(self, *seq): 
        """seq is a format codes, each of which represent how many
        bytes to take sequentially and print as an whole int, the 
        sign is always not coded by seq"""
        if not seq:
            seq = (1, 1, 1, 1, 1) #default is print byte by byte

        assert(sum(seq) == 5) #always print every byte
        
        vals = [0]*(len(seq) + 1) #1 for the sign
        vals[0] = self.sign
        currentByte = 1
        currentVal = 1

        for x in seq:
            vals[currentVal] = makeInt(self[currentByte:currentByte+x])
            currentByte = currentByte+x
            currentVal = currentVal+1

        return vals        
    def formatAssign(self, *args):
        self[0:6] = formatForAssign(*args)

class mix2byte (mixFullWord):
    def __init__(self):
        super(mix2byte, self).__init__()

class mixTape(object):
    def __init__(self, num):
        """`num' is the device number of the tape"""
        self.mixWordsPerBlock = 100
        self.octsPerBlock = self.mixWordsPerBlock * (1 + 10 + 1) # 1 for the sign, 10 for the magnitude, 1 for '\n', -0 is represented '-0000000000\n'

        try:
            self.file = open("tape%d" % num, "r+b")
        except IOError:
            self.file = open("tape%d" % num, "w+b")
    def ioc(self, mix):
        m = mix.getM()
        if m == 0:
            self.file.seek(0)
        elif m < 0:
            currentBlock = self.file.tell()/(self.octsPerBlock)
            seekBack = min(abs(m), currentBlock)
            self.file.seek(-seekBack*self.octsPerBlock, os.SEEK_CUR)
        elif m > 0:
            self.file.seek(m*self.octsPerBlock, os.SEEK_CUR) # a bug here, we must not seek over any blocks following the one last written
        pass
    def in_(self, mix):
        m = mix.getM()
        for x in range(0, self.mixWordsPerBlock):
            sign = self.file.read(1)
            if sign == '+':
                mix.memory[m+x].sign = 1
            elif sign == '-':
                mix.memory[m+x].sign = -1
            else:
                raise RuntimeError, 'read an invalid sign'
            
            mag = int(self.file.read(10))
            mix.memory[m+x].magnitude = mag
            self.file.read(1) #for the '\n' at the end of every word

    def out(self, mix):
        m = mix.getM()
        for x in range(0, self.mixWordsPerBlock):
            sign = '+' if mix.memory[m+x].sign == 1 else '-'
            mag = '%010d' % mix.memory[m+x].magnitude
            self.file.write(sign)
            self.file.write(mag)
            self.file.write('\n')
            self.file.flush()


class mixDisk(mixTape):
    def __init__(self, num):
        self.mixWordsPerBlock = 100 #100 words
        self.octsPerBlock = self.mixWordsPerBlock*(12) #12 = 10+1+1
        """`num' is the device number of the disk/drum"""
        try:
            self.file = open("disk%d" % num, "r+b")
        except IOError:
            self.file = open("disk%d" % num, "w+b")
    def ioc(self, mix):
        whichBlock = mix.xreg.value 
        self.file.seek(whichBlock * self.octsPerBlock)

    def in_(self, mix):
        self.ioc(self, mix)
        super(mixDisk, self).in_(self, mix)

    def out(self, mix):
        self.ioc(self, mix)
        super(mixDisk, self).out(self, mix)


class mixCardReader():
    def __init__(self):
        self.mixWordsPerBlock = 16
        self.charsPerBlock = self.mixWordsPerBlock*5
        try:
            self.file = open("card_reader", "r+b")
        except IOError:
            self.file = open("card_reader", "w+b")
    def ioc(self, mix):
        raise RuntimeError, 'ioc on card reader is not implemented'
    def in_(self, mix):
        m = mix.getM()
        block = self.file.readline()
        block = block.strip('\r\n')
        block = block + ' '*(self.charsPerBlock - len(block))

        for x in range(0, self.mixWordsPerBlock):
            mix.memory[m+x].sign = 1
            asciiChars = block[x*5: x*5+5]
            mix.memory[m+x][1:6] = [int(x) for x in ascii2mix_n(asciiChars)]

class mixCardPunch():
    def __init__(self):
        self.mixWordsPerBlock = 16
        self.charsPerBlock = self.mixWordsPerBlock*5
        try:
            self.file = open("card_punch", "r+b")
        except IOError:
            self.file = open("card_punch", "w+b")

    def ioc(self, mix):
        raise RuntimeError, 'ioc on card punch is not implemented'

    def out(self, mix):
        m = mix.getM()
        for x in range(0, self.mixWordsPerBlock):
            mixBytes = mix.memory[m+x][1:6]
            mixStr = ''.join([chr(x) for x in mixBytes])
            asciiStr = mix2ascii_n(mixStr)
            self.file.write(asciiStr)
        self.file.write('\n')
        self.file.flush()
        
        
class mixLinePrinter(mixCardPunch):
    def __init__(self):
        self.mixWordsPerBlock = 24
        self.charsPerBlock = self.mixWordsPerBlock*5
        self.file = sys.stdout

class mixTerminal(mixCardReader):
    def __init__(self):
        self.mixWordsPerBlock = 14
        self.charsPerBlock = self.mixWordsPerBlock*5
        self.file = sys.stdin

class mixPaperTape(mixCardReader, mixCardPunch): #will use the in_ and out from there
    def __init__(self):
        self.mixWordsPerBlock = 14
        try:
            self.file = open("paper_tape", "r+b")
        except IOError:
            self.file = open("paper_tape", "w+b")
    def ioc(self, mix):
        self.file.seek(0)

if __name__ == '__main__':
    def main():
        m = MixMachine()
        m.memory[1000] = mixFullWord(1, 1, 1, 1, 1, 1)
        m.areg.formatAssign(1, 1, 1, 1, 1, 1)
        m.ip = 2000
        m.memory[2000] = mixFullWord(1, 15, 40, 1, 5, 3)
        m.exec_1()
        print m.ip     
        print m.areg, m.regs[0], 'hello areg'
        print m.xreg

        m.memory[1000] = mixFullWord(-1, 0, 0, 0, 0)
        m.ip = 2000
        m.exec_1()
        print m.areg
        print m.xreg

        areg = m.areg
        areg.formatAssign(1, 2, 3, 4, 5, 6)
        print 'print areg1'
        print areg, m.regs[0]
        areg[0:6] = [1]+formatForAssign((1234,2),)+[1]+formatForAssign((150, 2),)
        m1000 = m.memory[1000]
        m1000[0:6] = [1]+formatForAssign((100,2),)+[5]+formatForAssign((50, 2),)
        
        m2000 = m.memory[2000]
        m2000[0:6] = [1]+formatForAssign((1000,2),)+[1, 5, 1]

        m.ip = 2000
        m.exec_1()
        print m.areg.sign, makeInt(m.areg[1:3]), m.areg[3], makeInt(m.areg[4:6])

        areg[0:6] = [-1]+formatForAssign((1234,2),)+[0, 0, 9]
        m1000[0:6] = [-1]+formatForAssign((2000, 2),)+formatForAssign((150, 2),)+[0]
        m2000[0:6] = [1]+formatForAssign((1000, 2),)+[1, 5, 2] #sub 1000
        m.ip = 2000
        m.exec_1()
        print m.areg.sign, makeInt(m.areg[1:3]), makeInt(m.areg[3:5]), areg[5]


        m1000[0:6] = formatForAssign(-1, 2, (1234, 4))
        m2000[0:6] = formatForAssign(1, (1000, 2), 1, 9, 3)
        areg[0:6] = formatForAssign(-1, (112, 5))
        m.ip = 2000
        m.exec_1()
        xreg = m.xreg
        print areg.formatForPrint(5), xreg.formatForPrint(5)

        areg.formatAssign(-1, 50, 0, (112, 2), 4)
        m1000.formatAssign(-1, 2, 0, 0, 0, 0)
        m2000.formatAssign(1, (1000, 2), 1, 5, 3)
        m.exec_1(2000)

        areg.formatAssign(-1, (0, 5))
        m1000.formatAssign(1, (_maxWordVal, 5))
        m2000.formatAssign(1, (1000, 2), 1, 5, 1)
        m.exec_1(2000)

        areg.formatAssign(-1, (0, 5))
        xreg.formatAssign(1, (1235,2), 0, 3, 1)
        
        m1000.formatAssign(-1, 0, 0, 0, 2, 0)
        m2000.formatAssign(1, (1000, 2), 1, 5, 4)

        print areg, xreg.formatForPrint(2, 1, 1, 1), m1000
        m.exec_1(2000)
        print areg.formatForPrint(1,2,1,1), xreg.formatForPrint(), m.overflow

        areg.formatAssign(-1, 0, 0, 31, 32, 39)
        xreg.formatAssign(1, 37, 57, 47, 30, 30)
        m2001 = m.memory[2001]
        m2002 = m.memory[2002]

        m2000.formatAssign(1, 0, 0, 0, 0, 5)
        m2001.formatAssign(1, (1, 2), 1, 0, 48)
        m2002.formatAssign(1, 0, 0, 0, 1, 5)

        m.exec_1(2000)
        print areg.formatForPrint(5), xreg.formatForPrint()
        m.exec_1()
        print areg.formatForPrint(5), xreg.formatForPrint()
        m.exec_1()
        print areg.formatForPrint(), xreg.formatForPrint()

        m2003 = m.memory[2003]
        m2004 = m.memory[2004]
        m2005 = m.memory[2005]

        m2001.formatAssign(1, 0, 1, 1, 3, 6)
        m2002.formatAssign(1, 0, 2, 1, 0, 6)
        m2003.formatAssign(1, 0, 4, 1, 5, 6)
        m2004.formatAssign(1, 0, 2, 1, 1, 6)
        m2005.formatAssign(1, (501, 2), 1, 4, 6)
        
        areg.formatAssign(1, 1, 2, 3, 4, 5)
        xreg.formatAssign(-1, 6, 7, 8, 9, 10)

        for x in range(2001, 2006):

            m.exec_1(x)
            print 'print areg'
            print areg.formatForPrint(), m.regs[0].formatForPrint()

        m.i1reg.formatAssign(1, (2000, 5))
        m2006 = m.memory[2006]
        m2007 = m.memory[2007]

        m2001.formatAssign(1, 0, 0, 1, 5, 8)
        m2002.formatAssign(1, 0, 0, 1, 13, 8)
        m2003.formatAssign(1, 0, 0, 1, 29, 8)
        m2004.formatAssign(1, 0, 0, 1, 3, 8)
        m2005.formatAssign(1, 0, 0, 1, 36, 8)
        m2006.formatAssign(1, 0, 0, 1, 0, 8)
        m2007.formatAssign(1, 0, 0, 1, 9, 8)

        m2000.formatAssign(-1, (80, 2), 3, 5, 4)
        for x in range(2001, 2008):
            m.exec_1(x)
            print areg.formatForPrint(), m.regs[0].formatForPrint()

        m2000.formatAssign(-1, 1, 2, 3, 4, 5)
        areg.formatAssign(1, 6, 7, 8, 9, 0)
        print 'hello world m.regs[0]'
        print m.regs[0], areg, m.areg
        areg.formatAssign(1, 6, 7, 8, 9, 0)
        m2001.formatAssign(1, 0, 0, 1, 5, 24)
        m2002.formatAssign(1, 0, 0, 1, 13, 24)
        m2003.formatAssign(1, 0, 0, 1, 45, 24)
        m2004.formatAssign(1, 0, 0, 1, 18, 24)
        m2005.formatAssign(1, 0, 0, 1, 19, 24)
        m2006.formatAssign(1, 0, 0, 1, 1, 24)
        m2007.formatAssign(1, (2000, 2), 2, 18, 37)

        for x in range(2001, 2007):
            m2000.formatAssign(-1, 1, 2, 3, 4, 5)
            m.exec_1(x)
            print m2000.formatForPrint()

        print 'hello world'

        for x in range(2000, 2008):
            print m.memory[x].formatForPrint()

        m.exec_1(2007)

        

        

    main()
