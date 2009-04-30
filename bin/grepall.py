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

find_pre_finds = [
    '-false', ]

find_custom_prunes = []
find_custom_finds = []
grep_options = []
grep_pattern = ''
grep_dirs = []


def parse_args():
    argsList = sys.argv[1:]
    i = 0
    while i < len(argsList):
        theArg = argsList[i]
        i = i + 1
        if theArg == '-f':
            parse_custom_finds(argsList[i])
            i = i + 1
        elif theArg == '-p':
            parse_custom_prunes(argsList[i])
            i = i + 1
        elif theArg == '-e':
            global grep_pattern
            grep_pattern = argsList[i]
            i = i + 1
        elif theArg[0:1] == '-':
            global grep_options
            grep_options.append(theArg)
        else:
            global grep_dirs
            grep_dirs.append(os.path.realpath(theArg))

def parse_custom_prunes(arg):
    find_custom_prunes[:] = []
    customize_find(arg, find_custom_prunes)

def parse_custom_finds(arg):
    find_custom_finds[:] = []
    customize_find(arg, find_custom_finds)

def customize_find(arg, findList, split=1):
    
    if split == 1:
        args = arg.split()
        for x in args:
            customize_find(x, findList, 0)
        return

    if arg[0:2] == ':f':
        findList.extend(['-o', '-iname', arg[2:]])
    elif arg[0:2] == ':l':
        for x in open('/q/etc/grepall/'+arg[2:]):
            customize_find(x, findList, 1)
    elif arg[0:1] == '.': #this is a suffix
        findList.extend(['-o', '-iname', '*' + arg])
    else:
        findList.extend(['-o', '-iname', arg])
        
    
def getFindCacheName(absDir):
    path = os.environ['HOME']\
        + '/.grepall/'\
        + absDir + '/' 

    

    return path + \
        grepAllCheckSum('P' +
                        ''.join(find_custom_prunes[2::3]) + 'F' +
                        ''.join(find_custom_finds[2::3]))

def getGrepCacheName(absDir):
    path = os.environ['HOME']\
        +'/.grepall/'\
        + absDir + '/'

    return path + \
        grepAllCheckSum('P' + 
                        ''.join(find_custom_prunes[2::3]) + 'F' +
                        ''.join(find_custom_finds[2::3]) + 'G' +
                        ''.join(grep_options[:]) + 
                        ''.join(grep_pattern))

def doFind(absDir, findCache):
    subprocess.call(('mkdir', '-p', os.path.dirname(findCache)))

    cacheFile = open(findCache, "w")
    #\( "${find_pre_prunes[@]}" "${prune_options[@]}" \) -prune -o -type f  \( "${find_options[@]}" \) -print 2>/dev/null >"$cache_find"
    find_command = ['find', absDir, '(' ] + find_pre_prunes \
        + find_custom_prunes + [')', '-prune', '-o', '-type', 'f', '(', '-false' ] \
        + find_custom_finds + [')', '-print']

    print find_command
    subprocess.call(find_command, stdout=cacheFile)
    cacheFile.close()

def updateFind(absDir, findCache):
    cacheFile = open(findCache + ".new", "w")
    find_command = ['find', absDir, '-newer', findCache, '(', '(' ] + find_pre_prunes \
        + find_custom_prunes + [')', '-prune', '-o', '-type', 'f', '(', '-false' ] \
        + find_custom_finds + [')', '-print', ')']

    subprocess.Popen(find_command, stdout = cacheFile)
    cacheFile.close()

def doGrep(absDir, findCache, grepCache):
    findList = open(findCache, "r").read().split('\n')

    grepCommand = ['grep', '-e', grep_pattern, '-H', '-n', ] +\
        grep_options +\
        findList

    cacheFile = open(grepCache, "w")
    cmdPipe = subprocess.Popen(grepCommand, stdout=subprocess.PIPE, stderr=open("/dev/null"))
    while True:
        line = cmdPipe.stdout.readline()
        if not line:
            break;
        sys.stdout.write(line)
        cacheFile.write(line)
    cacheFile.close()
        

def isOlder(file1, file2): #TODO: true if file1 older than file2 
    return True

def catGrep(grepCache):
    #subprocess.call(('cat', grepCache))

    #we will do something more sophisticated
    for line in open(grepCache, 'r'):
        fileName = line.split(':', 1)[0]
        if os.path.exists(fileName) and isOlder(fileName, grepCache):
            sys.stdout.write(line)
    
                    
if __name__ == '__main__':
    parse_args()
    for d in grep_dirs:
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

        
