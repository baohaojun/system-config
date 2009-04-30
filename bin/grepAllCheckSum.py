#!/usr/bin/python

def grepAllCheckSum(arg):
    charList = []
    for c in arg:
        if c in '*%\\/?><|:\"\n\t':
            charList.extend( list("%%%02x" % ord(c)) )
        else:
            charList.append(c)

    charSum = 0
    for c in charList:
        charSum = (charSum*31 + ord(c)) & 0xffffffff
        
    charList.append(".%08x" % charSum)
    return ''.join(charList)

if __name__ == '__main__':
    import sys
    print grepAllCheckSum(sys.argv[1])
        
