#!/usr/bin/python 

from grepAllCheckSum import *
import sys, os

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
    '-o', '-path', '"*/semantic.cache"', ]

find_pre_finds = [
    '-false', ]

find_custom_prunes = ['hello', 'world']
find_custom_finds = []
grep_options = []


def parse_args():
    argsList = sys.argv[1:]
    i = 0
    while i < len(argsList):
        theArg = argsList[i]
        i = i + 1
        if theArg == '-find':
            parse_custom_finds(argsList[i])
            i = i + 1
        elif theArg == '-prune':
            parse_custom_prunes(argsList[i])
            i = i + 1

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
    else:
        findList.extend(['-o', '-iname', arg])
        
                        
    


if __name__ == '__main__':
    parse_args()
    print find_custom_prunes
