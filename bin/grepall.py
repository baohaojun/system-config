#!/usr/bin/python 
import pdb
from grepAllCheckSum import *
import sys, os
from subprocess import *

find_pre_prunes = [
    '-path', '*/CVS', 
    '-o', '-path', '*/.svn', 
    '-o', '-path', '*/autom4te.cache', 
    '-o', '-path', '*/{arch}', 
    '-o', '-path', '*/.hg', 
    '-o', '-path', '*/_darcs', 
    '-o', '-path', '*/.git', 
    '-o', '-path', '*/.bzr', 
    '-o', '-path', '*~*', 
    '-o', '-path', '*#', 
    '-o', '-path', '*/TAGS', 
    '-o', '-path', '*/tags',
    '-o', '-path', '*/semantic.cache', 
    '-o', '-iname', '*.o',
    '-o', '-iname', '*.class', 
    '-o', '-iname', '*.obj',
    '-o', '-iname', '*.pyc',
    '-o', '-iname', '*.elc',
    '-o', '-path', '*/.ignore',
    '-o', '-path', '*/.repo',
    '-o', '-path', '*/.deps',
    ]


def isOlder(file1, file2): #TODO: true if file1 older than file2 
    return True


class GrepAllParse:
    def __init__(self):
        self.prunes = []
        self.finds = ['-o', '-true']
        self.grepOpts = []
        self.pattern = ''

        self.dirs = set(())

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
            elif theArg == '-cd': #clear dirs
                self.dirs = set(())
            elif theArg[0:1] == '-':
                self.grepOpts.append(theArg)
            else:
                self.dirs.add(os.path.realpath(theArg))

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
            for x in open(os.environ['HOME'] + '/etc/grepall/'+arg[2:]):
                self.customize_find(x, findList, 1)
        elif arg[0:1] == '.': #this is a suffix
            findList.extend(['-o', '-iname', '*' + arg])
        else:
            findList.extend(['-o', '-iname', arg])

class GrepAll:
    """there can be 3 cases about findCache (fc), grepCache (gc):
    1. no fc, no gc
    2. has fc, no gc
    3. has fc, has gc
    In case 1, we do a complete find
    In case 2, we do newer than fc find
    In case 3, we do newer than gc find
    After the program complete, the fc will contain all and only those possible files at the time"""    
    NO_FC_NO_GC = 1
    HAS_FC_NO_GC = 2
    HAS_FC_HAS_GC = 3
    def __init__(self, dir_, parser):
        self.prunes = parser.prunes
        self.finds = parser.finds
        self.grepOpts = parser.grepOpts
        self.pattern = parser.pattern

        self.cacheDir = os.environ['HOME'] + '/tmp/.grepall/' + dir_ + '/'
        call(('mkdir', '-p', self.cacheDir))
        self.workDir_ = dir_
        self.findCache = self.cacheDir + \
            grepAllCheckSum('P' +
                            ''.join(self.prunes[2::3]) + 'F' +
                            ''.join(self.finds[2::3]))

        self.newerFindCache = self.findCache + '.new'

        self.grepCache = self.cacheDir + \
            grepAllCheckSum('GP' + 
                            ''.join(self.prunes[2::3]) + 'F' +
                            ''.join(self.finds[2::3]) + 
                            ''.join(self.grepOpts[:]) + 
                            ''.join(self.pattern))
        self.tmpGrepCache = self.grepCache + '.new'

        self.findCommand = ['find', dir_, '(' ] + find_pre_prunes \
            + self.prunes + [')', '-prune', '-o', '-type', 'f', '(', '-false' ] \
            + self.finds + [')', '-print']

        self.newerFindCommand = ['find', dir_, '(' ] + find_pre_prunes \
            + self.prunes + [')', '-prune', '-o', '-type', 'f', '-newer', self.findCache, '(', '-false' ] \
            + self.finds + [')', '-print']

        self.newerGrepCommand = self.newerFindCommand[:]
        i = self.newerGrepCommand.index(self.findCache)
        self.newerGrepCommand[i] = self.grepCache

        self.grepCommand = ['grep', 
                            '-e', 
                            self.pattern,
                            '-H', 
                            '-n',
                            ] + \
                            self.grepOpts
        self.findPipe = None
        self.grepPipe = None
        self.reGrepSet = []
        self.grepFiles = []
        if not os.path.exists(self.findCache):
            if not os.path.exists(self.grepCache):
                self.state = GrepAll.NO_FC_NO_GC
                print 'no fc, no gc'
            else:
                raise RuntimeError, 'no fc, has gc'
        else:
            if not os.path.exists(self.grepCache):
                print 'has fc, no gc'
                self.state = GrepAll.HAS_FC_NO_GC
            else:
                print 'has fc, has gc'
                self.state = GrepAll.HAS_FC_HAS_GC
                s = os.stat(self.grepCache)
                self.grepCacheMtime = s.st_mtime
    def doFind(self):
        if self.state == GrepAll.NO_FC_NO_GC:
            self.findPipe = Popen(self.findCommand, stdout=open(self.findCache, 'w'), stdin=open("/dev/null", "r"))
        elif self.state == GrepAll.HAS_FC_NO_GC:
            self.findPipe = Popen(self.newerFindCommand, stdout=open(self.newerFindCache, 'w'), stdin=open("/dev/null", "r"))
        elif self.state == GrepAll.HAS_FC_HAS_GC:
            self.findPipe = Popen(self.newerGrepCommand, stdout=open(self.newerFindCache, 'w'), stdin=open("/dev/null", "r"))
        else:
            raise RuntimeError, 'no fc, has gc'

    def doGrep(self):
        if self.state == GrepAll.NO_FC_NO_GC: #we can't do grep when we has no FC
            pass
        elif self.state == GrepAll.HAS_FC_NO_GC:
            self.grepFiles = open(self.findCache, 'r').read().split('\n')
            if not self.grepFiles[-1]:
                del self.grepFiles[-1]
            self.grepPipe = Popen(self.grepCommand + self.grepFiles, stdout=PIPE, stdin=open("/dev/null", "r"), stderr=open('/dev/null', 'w'))
            tmpGCFile = open(self.tmpGrepCache, 'w')
            while True:
                line = self.grepPipe.stdout.readline()
                if not line:
                    break
                sys.stdout.write(line)
                sys.stdout.flush()
                tmpGCFile.write(line)
            tmpGCFile.close()
        else: #has GC, has FC
            lastFileName = ''
            lastFileState = ''
            tmpGCFile = open(self.tmpGrepCache, 'w')
            for line in open(self.grepCache, 'r'):
                fileName = line.split(':', 1)[0]
                if fileName == lastFileName:
                    if lastFileState == 'cat':
                        sys.stdout.write(line)
                        sys.stdout.flush()
                        tmpGCFile.write(line)
                    else:
                        #discard or regrep, no need to do anything
                        pass
                else:
                    lastFileName = fileName
                    if not os.path.exists(fileName) or self.needReGrep(fileName):
                        lastFileState = 'pass'
                        pass
                    else:
                        lastFileState = 'cat'
                        sys.stdout.write(line)
                        sys.stdout.flush()
                        tmpGCFile.write(line)
            tmpGCFile.close()

    def needReGrep(self, fileName):
        s = os.stat(fileName)
        return s.st_mtime > self.grepCacheMtime
    def waitFind(self):
        if self.findPipe:
            self.findPipe.wait()
            self.findPipe = None
        else:
            return
        print 'wait find done'
        if self.state == GrepAll.NO_FC_NO_GC:
            self.state = GrepAll.HAS_FC_NO_GC
            self.doGrep()
            return #no need to update FC
        elif self.state == GrepAll.HAS_FC_NO_GC:
            if not self.grepFiles:
                self.grepFiles = open(self.findCache, "r").read().split('\n')
            self.grepFiles = set(self.grepFiles)

            self.reGrepSet = open(self.newerFindCache, "r").read().split('\n')
            for x in self.reGrepSet[:]:
                if x in self.grepFiles:
                    self.reGrepSet.remove(x)
                else:
                    self.grepFiles.add(x)
        elif self.state == GrepAll.HAS_FC_HAS_GC:
            if not self.grepFiles:
                self.grepFiles = open(self.findCache, "r").read().split('\n')
            self.grepFiles = set(self.grepFiles)
            self.reGrepSet = open(self.newerFindCache, "r").read().split('\n')
            for x in self.reGrepSet[:]:
                self.grepFiles.add(x)

        #now let's update FC
        self.grepFiles = list(self.grepFiles)
        self.grepFiles.sort()
        findCacheFile = open(self.findCache, "w")
        for x in self.grepFiles:
            if x:
                findCacheFile.write(x)
                findCacheFile.write('\n')
                
        findCacheFile.close()
        os.remove(self.newerFindCache)

    def waitGrep(self):
        if self.grepPipe:
            self.grepPipe.wait()
            self.grepPipe = None

        if self.reGrepSet:
            if not self.reGrepSet[-1]:
                del self.reGrepSet[-1]
        if self.reGrepSet:
            self.grepPipe = Popen(self.grepCommand + self.reGrepSet, stdout=PIPE, stdin=open('/dev/null', 'r'))
            tmpGCFile = open(self.tmpGrepCache, 'a')
            while True:
                line = self.grepPipe.stdout.readline()
                if not line:
                    break;
                sys.stdout.write(line)
                sys.stdout.flush()
                tmpGCFile.write(line)
            self.grepPipe.wait()
            self.grepPipe = None
        os.rename(self.tmpGrepCache, self.grepCache)
            
if __name__ == '__main__':
    def main():
        parser = GrepAllParse()
        dirGrepDict = {}
        for x in parser.dirs:
            dirGrepDict[x] = GrepAll(x, parser)

        for x in parser.dirs:
            dirGrepDict[x].doFind()

        for x in parser.dirs:
            dirGrepDict[x].doGrep()

        for x in parser.dirs:
            dirGrepDict[x].waitFind()

        for x in parser.dirs:
            dirGrepDict[x].waitGrep()

    main()

