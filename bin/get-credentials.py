#!/usr/bin/python
import os
def getcred(host, user, port=0):
    file = open(os.path.expanduser("~/.authinfo"))
    while True:
        line = file.readline()
        if not line:
            break
        fields = line.split()
        if fields[1] == host and fields[3] == user:
            return fields[5]

if __name__ == '__main__':
    getcred("localhost", "bhj", 143)
