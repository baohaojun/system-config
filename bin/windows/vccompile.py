#!/bin/python
import sys, os, subprocess, re

def getFirstProject(slnFile):
    return getAllProjects(slnFile)[0]

def getAllProjects(slnFile):
    projects = []
    for line in open(slnFile):
        if line.find('Project') == 0:
            projects.append(line.split('"')[3])

    return projects

def getProjDir(slnFile, project):
    reobj = re.compile('^Project.*"' + project, re.I)
    for line in open(slnFile):
        line = '/'.join(line.split('\\'))
        if reobj.search(line):
            prjFile = line.split('"')[5]
            slnDir = os.path.dirname(slnFile)
            slnDir = os.path.abspath(slnDir)
            prjDir = os.path.dirname(prjFile)
            prjDir = os.path.join(slnDir, prjDir)
            return prjDir

def msdev(*options):
    vcExec = ''.join(options[0:1])
    options = options[1:]

    slnFile = ''.join(options[0:1])
    options = options[1:]

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

    cmd = ''.join(options[0:1])
    options = options[1:]

    if not cmd:
        cmd = '/Build'
    
    for p in prj:

        print "make: Entering directory `%s'" % getProjDir(slnFile, p)
        sys.stdout.flush()

        for c in cfg:
            print vcExec, slnFile, '/Project', p, '/Build', c, options
            subprocess.check_call((vcExec, slnFile, '/Project', p, cmd, c) + options)
        
        print "make: Leaving directory `%s'" % getProjDir(slnFile, p)
        sys.stdout.flush()


msdev(*sys.argv[1:])
