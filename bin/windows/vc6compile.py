#!/bin/python
import sys, os, subprocess, re

def getFirstProject(slnFile):
    return getAllProjects(slnFile)[0]

def getAllProjects(slnFile):
    projects = []
    for line in open(slnFile):
        if line.find('Project') == 0:
            projects.append(line.split('"')[1])

    return projects

def getProjDir(slnFile, project):
    reobj = re.compile('^Project.*"' + project, re.I)
    reobj2 = re.compile('^Project: ".*"=(.*) - Package', re.I)
    for line in open(slnFile):
        line = '/'.join(line.split('\\'))
        if reobj.search(line):
            matchObj = reobj2.search(line)
            prjFile = matchObj.groups()[0]
            slnDir = os.path.dirname(slnFile)
            slnDir = os.path.abspath(slnDir)
            prjDir = os.path.dirname(prjFile)
            prjDir = os.path.join(slnDir, prjDir)
            return prjDir

def msdev(slnFile, *options):
    prj = ''.join(options[0:1])
    options = options[1:]
    if not prj or prj == 'all':
        prj = getAllProjects(slnFile)
    elif re.search('^[0-9]*$', prj):
        prj = getAllProjects(slnFile)[int(prj)]
    else:
        prj = [prj]

    cfg = ''.join(options[0:1])
    options = options[1:]
    if not cfg:
        cfg = ['release']
    elif cfg == 'all':
        cfg = ['release', 'debug']
    else:
        cfg = [cfg]

    for p in prj:

        print "make: Entering directory `%s'" % getProjDir(slnFile, p)
        sys.stdout.flush()

        for c in cfg:
            #print ('vc8.com', slnFile, '/Project', p, '/Build', c)
            subprocess.check_call(('vc6.com', slnFile, '/make', "%s - win32 %s" % (p, c)) + options)
        
        print "make: Leaving directory `%s'" % getProjDir(slnFile, p)
        sys.stdout.flush()


msdev(*sys.argv[1:])
