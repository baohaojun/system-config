#!/usr/bin/python 

from grepAllCheckSum import *
import sys, os, subprocess

find_pre_prunes = [
    '-path', '"*/CVS"', '-o', '-path', '"*/.svn"', 
    '-o', '-path', '"*/autom4te.cache"', 
    '-o', '-path', '"*/{arch}"', 
    '-o', '-path', '"*/.hg"', 
    '-o', '-path', '"*/_darcs"', 
    '-o', '-path', '"*/.git"', 
    '-o', '-path', '"*/.bzr"', 
    '-o', '-path', '"*~*"', 
    '-o', '-path', '"*#"', 
    '-o', '-path', '"*/TAGS"', 
    '-o', '-path', '"*/semantic.cache"', 
    '-o', '-iname', '*.o',
    '-o', '-iname', '*.class', 
    '-o', '-iname', '*.obj',
    '-o', '-iname', '*.pyc',
    ]


def isOlder(file1, file2): #TODO: true if file1 older than file2 
    return True


class GrepAllParse:
    def __init__(self):
        self.prunes = []
        self.finds = []
        self.grepOpts = []
        self.pattern = ''

        self.dirs = []

        self.parse_args()

    def parse_args(self):
        argsList = sys.argv[1:]
        i = 0
        while i < len(argsList):
            theArg = argsList[i]
            i = i + 1
            if theArg == '-f':
                self.getFinds(argsList[i])
                i = i + 1
            elif theArg == '-p':
                self.getPrunes(argsList[i])
                i = i + 1
            elif theArg == '-e':
                self.pattern = argsList[i]
                i = i + 1
            elif theArg[0:1] == '-':
                self.grepOpts.append(theArg)
            else:
                self.dirs.append(os.path.realpath(theArg))

    def getPrunes(self, arg):
        self.prunes[:] = []
        self.customize_find(arg, self.prunes)

    def getFinds(self, arg):
        self.finds[:] = []
        self.customize_find(arg, self.finds)

    def customize_find(self, arg, findList, split=1):

        if split == 1:
            args = arg.split()
            for x in args:
                self.customize_find(x, findList, 0)
            return

        if arg[0:2] == ':f':
            findList.extend(['-o', '-iname', arg[2:]])
        elif arg[0:2] == ':l':
            for x in open('/q/etc/grepall/'+arg[2:]):
                self.customize_find(x, findList, 1)
        elif arg[0:1] == '.': #this is a suffix
            findList.extend(['-o', '-iname', '*' + arg])
        else:
            findList.extend(['-o', '-iname', arg])

class GrepAll:
    def __init__(self, dir_, parser):
        self.prunes = parser.prunes
        self.finds = parser.finds
        self.grepOpts = parser.grepOpts
        self.pattern = parser.pattern

        self.cacheDir = os.environ['HOME'] + '/.grepall/' + dir_ + '/'
        self.workDir_ = dir_
        self.findCache = self.cacheDir + \
            grepAllCheckSum('P' +
                            ''.join(self.prunes[2::3]) + 'F' +
                            ''.join(self.finds[2::3]))

        self.grepCache = self.cacheDir + \
            grepAllCheckSum('P' + 
                            ''.join(self.prunes[2::3]) + 'F' +
                            ''.join(self.finds[2::3]) + 'G' +
                            ''.join(self.grepOpts[:]) + 
                            ''.join(self..pattern))

        self.findCommand = ['find', absDir, '(' ] + find_pre_prunes \
            + self.prunes + [')', '-prune', '-o', '-type', 'f', '(', '-false' ] \
            + self.finds + [')', '-print']

        self.updateFindCommand = ['find', absDir, '-newer', findCache, '(', '(' ] + find_pre_prunes \
            + self.prunes + [')', '-prune', '-o', '-type', 'f', '(', '-false' ] \
            + self.finds + [')', '-print', ')']

        self.grepCommand = ['grep', 
                            '-e', 
                            self.pattern,
                            '-H', 
                            '-n',
                            ] + \
                            self.grepOpts
         self.findPipe = None
         self.grepPipe = None

    def doFind(self):
        if os.path.exists(self.findCache):
            self.findPipe = subprocess.Popen(

    def doGrep(self, absDir, findCache, grepCache):

        findList = open(findCache, "r").read().split('\n')

        grepCommand = 

        cacheFile = open(grepCache, "w")
        cmdPipe = subprocess.Popen(grepCommand + findList, stdout=subprocess.PIPE, stderr=open("/dev/null"))
        while True:
            line = cmdPipe.stdout.readline()
            if not line:
                break;
            sys.stdout.write(line)
            cacheFile.write(line)

        findList = self.waitUpdateFindDone()
        if findList:
            cmdPipe = subprocess.Popen(grepCommand + findList, stdout=subprocess.PIPE, stderr=open("/dev/null"))
        while True:
            line = cmdPipe.stdout.readline()
            if not line:
                break;
            sys.stdout.write(line)
            cacheFile.write(line)
        cacheFile.close()


    def catGrep(self, grepCache):
        #subprocess.call(('cat', grepCache))

        #we will do something more sophisticated
        for line in open(grepCache, 'r'):
            fileName = line.split(':', 1)[0]
            if os.path.exists(fileName) and isOlder(fileName, grepCache):
                sys.stdout.write(line)
    
                    
if __name__ == '__main__':
    parse_args()
    for d in dirs:
        findCache = getFindCacheName(d) 
        print 'findCache is', findCache

        if os.path.exists(findCache):
            updateFind(d, findCache)
        else:
            doFind(d, findCache)

         grepCache = getGrepCacheName(d)
        print 'grepCache is', grepCache

        if False and os.path.exists(grepCache):
            catGrep(grepCache)
        else:
            doGrep(d, findCache, grepCache)
        # if fileExist(findCache):
        #     doNewerFind(findCache)
        #     uniqLines(findCache) #
        # else:
        #     doFind(d)

        # grepCache = getGrepCacheName(d)
        # if fileExist(grepCache):
        #     for srcFile in grepCache:
        #         if srcFile missing:
        #             remove the lines belong to srcFile in grepCache
        #         elif srcFile newer:
        #             regrep srcFile
        #         else:
        #             cat the srcFile grep

        #     for srcFile in newerFound:
        #         if srcFile already regreped:
        #             pass
        #         else:
        #             grep srcFile
        # else:
        #     doGrep(d)

        
