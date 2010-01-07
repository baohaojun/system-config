#!/bin/bash

#~/bin/windows/NetworkTest.exe& #this is ssh2Emacs
cd ~/doc
regedit /s ime-noctrlshift-noaltshift.reg


# MYXWINSH=~/bin/windows/lnks/myxwin.sh
# (echo "#!/bin/bash -l"; grep -v xterm /usr/bin/startxwin.sh|sed -ne '/^#.*/d; /^exit$/d; /./p'|grep -v checkX) > $MYXWINSH
# chmod +x $MYXWINSH
# myxwin.sh&
startxwin&
net start sshd&
(cd /e/cygwin-cvsroot && rsync -avz rsync://cygwin.com/src-cvs .)&

(
    while ! ssh bhj2 echo hello; do true; done; 
    ssh bhj2 'while true; do 
offlineimap
echo wait for 300 seconds
sleep 300; done'&
    
    ssh bhj2 'while true; do 
pop2imap --host1 localhost --user1 baohaojun@yahoo.com --port1 2000 --passfile1 .remote-auth --host2 localhost --user2 bhj --passfile2 .local-auth --folder hotmail.com --delete
echo wait for 300 seconds
sleep 300
done'&

)
