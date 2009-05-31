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

def printProjectDir(slnFile, project):
    reobj = re.compile('^Project.*"' + project, re.I)
    for line in open(slnFile):
        line = '/'.join(line.split('\\'))
        if reobj.search(line):
            prjFile = line.split('"')[5]
            slnDir = os.path.dirname(slnFile)
            slnDir = os.path.abspath(slnDir)
            prjDir = os.path.dirname(prjFile)
            prjDir = os.path.join(slnDir, prjDir)
            print "make: Entering directory `%s'" % prjDir
            sys.stdout.flush()
            return

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
    if not cfg or cfg == 'all':
        cfg = ['release', 'debug']
    else:
        cfg = [cfg]

    cmd = ''.join(options[0:1])
    options = options[1:]

    if not cmd:
        cmd = '/Build'
    
    for p in prj:
        printProjectDir(slnFile, p)
        for c in cfg:
            #print ('vc8.com', slnFile, '/Project', p, '/Build', c)
            subprocess.call(('vc8.com', slnFile, '/Project', p, cmd, c) + options)
            pass

msdev(*sys.argv[1:])



#     shift #get rid of sln_file
#     shift #get rid of project
#     shift #get rid of config

#     local cmd
#     if [[ -z "$1" ]]
#     then
#         cmd=/Build
#     else 
#         cmd="$1"
#         shift #it could be /Clean        
#     fi
    
#     #the remaining args could be like `/clean'
#     for p in $project; do
#         print_project_dir $sln_file $project #for Emacs, echo make: Entering directory `C:/src/alchemy/JSC-vc8/AlchemyLib'
#         for c in $config; do
#             echo "compiling project \`$p' with config \`$c' in \`$sln_file'"
#             vc8.com "$sln_file" /Project $p "$cmd" $c "$@"
#         done
#     done    
# }

# if ls *.sln > /dev/null 2>&1
# then
#     msdev ./*.sln  "$@" 
# elif ls ../*.sln > /dev/null 2>&1
# then
#     msdev ../*.sln  "$@"
# elif ls ../../*.sln > /dev/null 2>&1
# then
#     msdev ../../*.sln  "$@"
# fi
