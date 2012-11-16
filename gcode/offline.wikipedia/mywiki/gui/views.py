from django.http import *
import os, re, urllib, subprocess

def index(request):
    return article(request, "Wikipedia")

def article(request, lang, article):
    article = article.encode('utf-8')
    if article.endswith("/"):
        article = article[:-1]
    if article.endswith("/&redlink=1"):
        article = article[:-11]
    result = "Not found"
    print 'do article for', lang, ':', article
    for line in subprocess.Popen(("wiki-sorted-idx-title-query", lang, article.replace('_', ' ')), stdout=subprocess.PIPE).communicate()[0].split('\n'):
        print line
        res = re.match(r'^(\d+%)\s\[([^\t]+)\t' + r'(0x[0-9A-Fa-f]+)\s+' * 9 + r'\]$', line)
        if res != None and res.group(2) == article.replace('_', ' '):
            cmd = ["./show.pl", lang]
            for i in range(2, 12):
                cmd.append(res.group(i))
            
            print cmd
            result = subprocess.Popen(cmd, stdout=subprocess.PIPE).stdout.read()
            break
    else:
        return search(request, lang, article)
    return HttpResponse(result)

def do_dict(request, entry):
    entry = entry.encode('utf-8')
    result = "Not found"
    cmd = ["dict", entry]
    result = subprocess.Popen(cmd, stdout=subprocess.PIPE).stdout.read()
    return HttpResponse(result)

def do_dict_defs(request, entry):
    entry = entry.encode('utf-8')
    cmd = ["dict-defines", entry]
    result = subprocess.Popen(cmd, stdout=subprocess.PIPE).stdout.read()
    return HttpResponse(result)

def do_dict_defs_sub(request, entry, sub_entry):
    entry = entry.encode('utf-8')
    cmd = ["dict-defines", entry, sub_entry]
    result = subprocess.Popen(cmd, stdout=subprocess.PIPE).stdout.read()
    return HttpResponse(result)

def do_dict_match(request, entry):
    entry = entry.encode('utf-8')
    cmd = ["dict-matching", entry]
    result = subprocess.Popen(cmd, stdout=subprocess.PIPE).stdout.read()
    return HttpResponse(result)
    
def do_dict_matching_sub(request, entry, sub_entry):
    entry = entry.encode('utf-8')
    cmd = ["dict-matching", entry, sub_entry]
    result = subprocess.Popen(cmd, stdout=subprocess.PIPE).stdout.read()
    return HttpResponse(result)

def search(request, lang, article):
    if type(article) is str:
        print "Searching for article", article
    else:
        print "Searching for article", article.encode('utf-8')
    lines = []
    cmd = ['wiki-query-keywords', lang]
    cmd.extend(article.replace('_', ' ').split())
    print cmd
    for line in subprocess.Popen(cmd, stdout=subprocess.PIPE).communicate()[0].split('\n'):
        if line:
            lines.append(line[:]) 
    if len(lines) == 0:
        result = '<html><head><title>Wikipedia has nothing about this.</title>'
        result += '</head><body>Wikipedia has nothing about this.<br/>'
        result += 'You can keyword search about it  <a href="/keyword/%s">here</a>' % article
        result += '</body></html>'
    else:
        result = "<html><head><title>Choose one</title></head><body><h1>Choose one of the options below</h1>\n"
        for line in lines:
            print "line is ", line
            res = re.match(r'^(\d+%)\s\[([^\t]+)\t' + r'(0x[0-9A-Fa-f]+)\s+' * 9 + r'\]$', line)
            if res != None:
                result += "(%s) <A HREF=\"/%s/article/%s\">%s</A><br/>\n" % (res.group(1), lang, urllib.quote(res.group(2)).decode('utf-8'), res.group(2).decode('utf-8'))
                print "regexp is matched ok:"
            else:
                print "please check your regexp"
        result += "</body></html>"
    return HttpResponse(result)

def keyword(request, lang, article):
    if type(article) is str:
        print "Searching for article", article
    else:
        print "Searching for article", article.encode('utf-8')
    lines = []
    cmd = ["wiki-query-keywords", lang]
    for i in article.replace('_', ' ').split():
        cmd.append(i)
    print cmd
    for line in subprocess.Popen(cmd, stdout=subprocess.PIPE).communicate()[0].split('\n'):
        print line
        lines.append(line[:])
    if len(lines) == 0:
        result = '<html><head><title>No exact match for this article name found in Wikipedia (try a keyword search).</title>'
        result += '</body></html>'
    else:
        result = "<html><head><title>Choose one</title></head><body><h1>Choose one of the options below</h1>\n"
        for line in lines:
            print "line is ", line
            res = re.match(r'^(\d+%)\s\[([^\t]+)\t' + r'(0x[0-9A-Fa-f]+)\s+' * 9 + r'\]$', line)
            if res != None:
                result += "(%s) <A HREF=\"/%s/article/%s/\">%s</A><br/>\n" % (res.group(1), lang, urllib.quote(res.group(2)).decode('utf-8'), res.group(2).decode('utf-8'))
            else:
                print "res is null"
        result += "</body></html>"
    return HttpResponse(result)

def searchbar(request, lang):
    searchData = request.GET['data'].encode('utf-8')
    return keyword(request, lang, searchData)

def searchdict(request):
    searchData = request.GET['data'].encode('utf-8')
    return do_dict(request, searchData)

def searchdict_def(request):
    searchData = request.GET['data'].encode('utf-8')
    return do_dict_defs(request, searchData)

def searchdict_match(request):
    searchData = request.GET['data'].encode('utf-8')
    return do_dict_match(request, searchData)

# Local Variables: #
# tab-width: 4 #
# python-indent: 4 #
# End: #
 
