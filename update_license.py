import os
import sys
import re

rn = re.compile("\r\n$",re.MULTILINE)
n = re.compile("(?<!\r)\n",re.MULTILINE)

oldLicense = re.compile("^" + re.escape( "/****************************************************************************************" ) + ".*2011.*" +  re.escape("****************************************************************************************/" ),re.MULTILINE|re.DOTALL)

newLicense = """/****************************************************************************************
 * Copyright (c) 2011-2012 Patrick von Reth <patrick.vonreth@gmail.com>                 *
 *                                                                                      *
 * This program is free software; you can redistribute it and/or modify it under        *
 * the terms of the GNU General Public License as published by the Free Software        *
 * Foundation; either version 2 of the License, or (at your option) any later           *
 * version.                                                                             *
 *                                                                                      *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY      *
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A      *
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.             *
 *                                                                                      *
 * You should have received a copy of the GNU General Public License along with         *
 * this program.  If not, see <http://www.gnu.org/licenses/>.                           *
 ****************************************************************************************/"""
 
def myWalk(root):
    for  root,folders,files in os.walk(root):
        for folder in folders:
            if not (folder == ".git" or folder == ".svn"):
                myWalk(os.path.join(root,folder))
        for fileName in files:
            fileName = os.path.join(root,fileName)
            if os.path.isfile(fileName) and (fileName.endswith(".h") or fileName.endswith(".cpp")):
                f = open(fileName,"rb+")
                tmp = f.read()                
                f.close()
                tmp = str(tmp,"UTF-8")
                
                lineEnding = "\r\n"
                if re.search(rn,tmp) == None:
                   lineEnding = "\n"
                
                tmp = re.sub(oldLicense,newLicense,tmp)
                tmp = re.sub(n,lineEnding,tmp)
                #print(re.findall(oldLicense,tmp))
                f = open(fileName,"wb+")
                f.write(tmp.encode("UTF-8"))
                f.close()
                #sys.exit(0)
    
    

myWalk(sys.argv[1])
sys.exit(0)


         
