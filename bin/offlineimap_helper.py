#!/usr/bin/python
# -*- coding: utf-8 -*-
import re
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

def convert_amp_notation(code):
    char = 0
    bits = 0
    str = ""
    for c in code:
        c = ord(c)
        sum = 0;
        if ord('A') <= c and c <= ord('Z'):
            sum += c - ord('A')
        elif ord('a') <= c and c <= ord('z'):
            sum += c - ord('a') + 26
        elif ord('0') <= c and c <= ord('9'):
            sum += c - ord('0') + 52
        elif ord('+') == c:
            sum += 62
        elif ord(',') == c:
            sum += 63
        if bits + 6 < 16:
            char *= 64
            char += sum
            bits += 6
        else:
            char <<= 6 - (bits + 6 - 16)
            char += (sum >> (bits + 6 - 16))
            str += unichr(char)
            char = sum & ((1 << (bits + 6 - 16)) - 1)
            bits -= 10
    return str.encode('utf-8')

def ali_mailbox_conv(name):
    def re_helper(m):
        return convert_amp_notation(m.groups()[0])
    box = re.sub("&([^-]+)-", re_helper, name)
    print "got mailbox:", box, "from", name
    if box == "已发送":
        box = "Sent"
    elif box == "已发送邮件":
        box = "SentMail"
    elif box == '已删除邮件':
        box = "Deleted"
    elif box == "Sent" or box == "SentMail" or box == "Deleted":
        box = "HelloWorld-" + box
    return box

def ali_mailbox_conv_to_remote(name):
    if name == 'Sent':
        return "&XfJT0ZAB-"
    elif name == 'SentMail':
         return '&XfJT0ZABkK5O9g-'
    elif name == "Deleted":
        return '&XfJSIJZkkK5O9g-'
    elif name.find("HelloWorld-") == 0:
        return name.replace('HelloWorld-', '')
    return name

def ali_folder_match(folder, pats):
    folder = ali_mailbox_conv(folder).lower()
    if folder.find('/') >= 0:
        return False
    if folder.find("已删除邮件") >= 0:
        return False
    for pat in pats:
        if pat in folder:
            return True
    return False


if __name__ == '__main__':
    getcred("localhost", "bhj", 143)
    print ali_mailbox_conv("Outbox/&mZmZmZmZ--outbox-&mZk-")
