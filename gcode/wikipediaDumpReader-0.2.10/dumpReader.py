#!/usr/bin/env python
# -*- coding: utf-8 -*-
from PyQt4 import QtCore, QtGui
from PyQt4.QtCore import Qt
import os, sys
import PyQt4.uic
import re
import codecs
from xml.dom.minidom import parseString
from xml.parsers.expat import ExpatError
try:
	import mod.bz2 as bz2
except ImportError:
	import mod64.bz2 as bz2
import random
import mparser
import mathexp
import convert_idx_s

##
##	Main class : GUI
##

class MainViewer(QtGui.QDialog):
	def __init__(self,parent = None,name = None,modal = 0,fl = 0):
		# Some options :		
		self.loadTabInBackground = True
		latexRendering = True
		fontSize = 9
		self.smallerLayout = False
		keepMathImageFiles = False # if True, do not erase old images when starting the program
		# End
		
		debugButton = False # hide debugs stuffs, only for normal layout
		
		QtGui.QDialog.__init__(self, parent)
		self.checkBox1 = None
		self.textLabel1 = None
		if self.smallerLayout == True:
			PyQt4.uic.loadUi(os.path.join(os.path.dirname(__file__), 'form3_smaller.ui'), self)
		else:
			PyQt4.uic.loadUi(os.path.join(os.path.dirname(__file__), 'form3.ui'), self)
		self.checkBox3.setChecked(False)
		self.inputbz2Archive, self.outidxname, self.outblockname, self.outidxname_s = wikiDataProcessing()
		self.idx_s = self.outidxname_s and convert_idx_s.load_entrylist_table(self.outidxname_s)
		if "wiktionary" in self.inputbz2Archive:
			self.wiktionary = True
		else:
			self.wiktionary = False
		self.blocksaddr, self.bitaddr = readBlocksAddr(self.outblockname)
		self.cachedArticles = {}
		self.mathRendererInstance = None
		self.cache_font = QtGui.QFont()
		if fontSize > 0: # you may want to disable explicit font-setting if the .ui ones should prevail
			self.setFontSize(fontSize)		
		print self.checkBox1
		if debugButton == False:
			if self.checkBox1:
				self.checkBox1.setChecked(False)
				self.checkBox1.hide()
		elif self.checkBox1 != None:
			self.checkBox1.setChecked(False)
		if latexRendering:
			try:
				self.mathRendererInstance = mathexp.MathExp(keepMathImageFiles)
			except:
				print 'Error while loading math parser'
		#try:
		#	self.textEdit1.setOpenLinks(False)
		#except AttributeError:
		#	print "Qt < 4.3 found"
		titleEntry = u"Wikipedia"
		name = self.cacheArticle(titleEntry)
		self.displayCachedEntry(name)
	
	def setFontSize(self, size):
		self.cache_font.setPointSize(size)
		self.textEdit1.setFont(self.cache_font)
	
	def launchSearch(self):
		t = unicode(self.lineEdit1.text())
		if t == "":
			return
		print "LaunchSearch" , `t`
		t = "[%s%s]%s" % (t[0].upper() , t[0].lower() , t[1:])
		latin1 = t.encode('utf-8')
		idxname = self.outidxname.encode('utf-8')
		#print ('zgrep "' + latin1 + '" ' + idxname, 'r')
		p = os.popen('zgrep "' + latin1 + '" ' + idxname, 'r')
		matchlist = [x.decode('utf-8')[:-1] for x in p]
		matchlist = [x[:x.index('\t')] for x in matchlist]
		self.listBox1.clear()
		for w in matchlist:
			self.listBox1.addItem(w)
	
	
	def loadTextblockRaw(self, block, start,length):
		f = bz2.BZ2File( self.inputbz2Archive )
		L, olength = f.loadBlock(self.blocksaddr[block], self.bitaddr[block], start, length)
                if olength - start < length:
                    # If we're on a block boundary, continue to next file
		    L2, _ = f.loadBlock(self.blocksaddr[block+1], self.bitaddr[block+1], 0, length-(olength - start))
                    L  = L[:olength-start] + L2[:start + length-olength]
                try:
		    D = parseString("<page>\n" + L)
                except ExpatError:
                    return "Error : can't load this article - sorry", "error"
		
		n = D.getElementsByTagName('title')
		title = n[0].firstChild.nodeValue
		
		n = D.getElementsByTagName('text')
		t = n[0].firstChild.nodeValue
		return t, title
	
	def loadTextblock(self, block, start,length):
		""" read a block of wiki-text, then filter-it through the regex-made converter"""
		t, title = self.loadTextblockRaw(block, start, length)
		if self.checkBox1 and self.checkBox1.isChecked():
			self.textEdit2.setPlainText(t)
		
		if t[0:9].upper() != "#REDIRECT": # avoid filtering if a redirection
			t = filterMarkup(t, self.mathRendererInstance)
		return t, title
	
	def cacheARandomArticle(self):
		# TODO: make it cleaner, make it error-proof
		print "Choosing at random (this may be slow :-)"
		idxname = self.outidxname.encode('utf-8')
		if not hasattr(self, "numArticles"):
			self.numArticles = int(os.popen('zcat "'+idxname+'" | wc -l', 'r').readline()[:-1])
		# avoid choosing something containing":", avoid REDIRECT
		t, candidate = "", ":"
		while t[0:9].upper() == "#REDIRECT" or candidate.find(':') > -1:
			i = random.randint(1, self.numArticles)
			l = os.popen("zcat '"+idxname+"' | head -n %d | tail -n 1" % i).readline()
			block, offset, leng = [int(x) for x in l.split()[-3:]]
			t, candidate = self.loadTextblock(block, offset, leng)
		titleEntry = candidate
		self.cachedArticles[titleEntry] = t
		self.listBox3.insertItem(self.listBox3.currentRow() + 1, titleEntry)
		return titleEntry

	def cacheArticle(self, titleEntry):
		self.textEdit1.viewport().setCursor(QtCore.Qt.BusyCursor)
		self.setCursor(QtCore.Qt.BusyCursor)
		url = titleEntry.split('#')
		titleEntry = url[0] # todo : to do
		
		# special feature: load a random article.
		if titleEntry == "":
			return self.cacheARandomArticle()
		# when loading an article, title have uppercased first word
		if not titleEntry[0].isupper() and not self.wiktionary:
			titleEntry = titleEntry[0].upper() + titleEntry[1:]
		
		if not self.cachedArticles.has_key(titleEntry):
			latin1 = titleEntry.encode('utf-8')
			idxname = self.outidxname.encode('utf-8')
			#print ('zgrep "^' + latin1 + '\t" ' + idxname, 'r')
			if self.idx_s: # new style find
				l = convert_idx_s.load_entry_addr(latin1, self.idx_s, idxname) or ""
			else:
				l = os.popen('zgrep "^' + latin1 + '\t" ' + idxname, 'r').readline()
			# i don't utf_8_decode because i'm only interested in numbers
			try:
				block, offset, leng = [int(x) for x in l.split()[-3:]]
			except ValueError:
				if not self.smallerLayout:
					self.textLabel1.setText(u"Article Not Found : " + titleEntry)
				self.textEdit1.viewport().setCursor(QtCore.Qt.ArrowCursor)
				self.setCursor(QtCore.Qt.ArrowCursor)
				return None
			try:
				t, parsedtitle = self.loadTextblock(block, offset, leng)
			except StandardError:
				t = "Error while retrieving data from the dump for this article<br/>\nSorry<br/>\n(debug informations might be available in a terminal output)"
			
			if t[0:9].upper() == "#REDIRECT":
				destArticle = t[t.index('[[')+2:t.index(']]')]
				if destArticle != titleEntry:
					print '%s redirects to %s' % (`titleEntry`, `destArticle`)
					return self.cacheArticle(destArticle)
				else:
					print "WARNING: Redirection to itself (%s). Abort" % `titleEntry`
			
			self.cachedArticles[titleEntry] = t
			self.listBox3.insertItem(self.listBox3.currentRow() + 1, titleEntry)
		
		self.textEdit1.viewport().setCursor(QtCore.Qt.ArrowCursor)
		self.setCursor(QtCore.Qt.ArrowCursor)
		return titleEntry
		
	def displayCachedEntry(self, titleEntry):
		if not titleEntry:
			return
		if not self.smallerLayout:
			self.textEdit1.setHtml(self.cachedArticles[titleEntry])
			self.textLabel1.setText("<h2>%s</h2>" % titleEntry)
		else:
			self.textEdit1.setHtml(("<h2>%s</h2>" % titleEntry) + self.cachedArticles[titleEntry])
		self.currentlyDisplayedArticle = titleEntry
		
		i = self.listBox3.findItems(titleEntry, Qt.MatchExactly)[0]
		self.listBox3.setCurrentItem(i)
	
	@QtCore.pyqtSignature("")
	def on_pushButton9_clicked(self):
		self.closeCurrentPage()
	
	def closeCurrentPage(self):
		row = self.listBox3.currentRow()
		if self.listBox3.count() > 1:
			del self.cachedArticles[self.currentlyDisplayedArticle]
			self.listBox3.takeItem(row)
	
	def on_listBox3_currentTextChanged(self, a0):
		name = self.cacheArticle(unicode(a0))
		self.displayCachedEntry(name)
	
	def on_listBox1_currentTextChanged(self, a0):
		if not a0.isNull():
			self.lineEdit1.setText(a0)
	
	def on_listBox1_itemDoubleClicked(self, widgetitem):
		name = self.cacheArticle(unicode(widgetitem.text()))
	
	@QtCore.pyqtSignature("")
	def on_pushButton1_clicked(self):
		a0 = self.lineEdit1.text()
		name = self.cacheArticle(unicode(a0))
		self.displayCachedEntry(name)
	
	@QtCore.pyqtSignature("")
	def on_pushButton3_clicked(self):
		self.launchSearch()
	
	@QtCore.pyqtSignature("")
	def on_pushButton2_clicked(self):
		# debug button
		# execute the code on the regex
		k = filterMarkup(unicode(self.textEdit2.toPlainText()), self.mathRendererInstance)
		self.textEdit1.setHtml(k)
		self.textEdit3.setPlainText(k)
		return
		t = unicode(self.textEdit2.toPlainText())
		code = unicode(self.textEdit3.toPlainText())
		print "Before:", t
		print "Eval:, ", code
		exec(code)
		print "After:", t
		self.textEdit1.setHtml(t)
	
	def on_textEdit1_anchorClicked(self, qurl):
		#print "AnchorClicked", unicode(qurl.toString())
		name = self.cacheArticle(unicode(qurl.toString()))
		if not self.loadTabInBackground:
			self.displayCachedEntry(name)
	
	def on_lineEdit1_returnPressed(self):
		if not self.checkBox3.isChecked():
			self.pushButton1.click()
		else:
			self.pushButton3.click()


##
##	Wiki Archive IO settings & init
##

def readBlocksAddr(outblockname):
	#print "Loading block addr..."
	blocksaddr = []
	bitaddr = []
	for k in open(outblockname):
		a, b, c = k.split()
		blocksaddr.append(int(b))
		bitaddr.append(int(c))
	return blocksaddr, bitaddr


"""Please select a dump file from the list, or load a new one
You can download a dump from your language from the wikipedia web server at :
http://download.wikimedia.org/backup-index.html
They are files generally named like pages-articles.xml.bz2."""
class LoaderBox(QtGui.QDialog):
	def __init__(self, *kargs):
		QtGui.QDialog.__init__(self, *kargs)
		PyQt4.uic.loadUi(os.path.join(os.path.dirname(__file__), 'loader.ui'), self)
		self.connect(self.buttonBox.button(QtGui.QDialogButtonBox.Open), QtCore.SIGNAL("clicked()"), self.load)
		self.connect(self.buttonBox.button(QtGui.QDialogButtonBox.Ok), QtCore.SIGNAL("clicked()"), self.ok)
		self.conffile = os.path.join(os.environ['HOME'], '.wikipediadumpreaderrc')
		try:
			self.l = [x.rstrip() for x in codecs.open(self.conffile, encoding='utf-8') if x]
		except:
			self.l = []
		self.comboBox.addItems(self.l)
	def load(self):
		t = QtGui.QFileDialog.getOpenFileName(None, "Opening a dump", os.curdir, 'Wikipedia dump (*articles.xml.bz2 *.xml.bz2)')
		if not t.isNull():
			self.l.insert(0, unicode(t))
			self.comboBox.insertItem(0, t)
			self.comboBox.setCurrentIndex(0)
	def accept(self): pass
	def ok(self):
		if len(self.l):
			self.l = [self.l.pop(self.comboBox.currentIndex())] + self.l
			codecs.open(self.conffile, 'w', encoding='utf-8').write("\n".join(self.l))
			QtGui.QDialog.accept(self)

def wikiDataProcessing():
	if len(sys.argv) == 2:
		inputbz2Archive = sys.argv[1].decode('utf-8')
	else:
		v = LoaderBox()
		if v.exec_():
			inputbz2Archive = v.l[0]
			del v
		else:
			sys.exit(0)
	
	filesize = int(os.stat(inputbz2Archive)[6] / 1024 / 1024) + 1
	
	filename = inputbz2Archive
	if filename.endswith('.xml.bz2'):
			outidxname = filename[:-8] + '.idx.gz'
			outblockname = filename[:-8] + '.blocks.idx'
			outidxname_s = filename[:-8] + '.idx_s.gz'
	else:
		print "filename : ", `filename`
		print "file should be a wikipedia .xml.bz2 named file - Aborting"
		sys.exit(0)
	
	# Build index if needed
	index_exists = os.path.exists(outidxname)
	# First pass: build main (large) index
	if not index_exists or not os.path.exists(outblockname) or \
		os.stat(inputbz2Archive)[8] > os.stat(outidxname)[8]:
		
		dialogBuild = QtGui.QProgressDialog("Building the index", "Abort", 0, filesize)
		dialogBuild.show()
		
		def callback(currentPos):
			dialogBuild.setValue(currentPos)
			QtGui.qApp.processEvents()
			if (dialogBuild.wasCanceled()):
				QtGui.QMessageBox.critical(None, "Abort", "Index creation canceled - it may be incomplete or corrupt\nYou might want to manually delete the two files: \n%s\n%s" % (outidxname, outblockname))
				sys.exit(0)
		try:
			mparser.buildIndex(inputbz2Archive, outidxname, outblockname, callback)
		except EOFError, e:
			QtGui.QMessageBox.critical(None, "Index creation problem", unicode(e))
	# sanity check on the indexes files : must be >0 bytes, and must been both written within 1 second range
	s1, s2 = os.stat(outblockname), os.stat(outidxname)
	if s1[6] == 0 or s2[6] == 0 or abs(s1[8] - s2[8]) > 1:
		QtGui.QMessageBox.warning(None, "Continue anyway", "The index files are either empty or have different writing times - it may indicate a problem, such as an interrupted previous indexing.\nTo force re-indexing, you can manually delete the two files: \n%s\n%s.\nThe program will now continue anyway." % (outidxname, outblockname))

	# Second pass : entry-list sorting only
	if not os.path.exists(outidxname_s): # interactive idx_s creation
		if index_exists and QtGui.QMessageBox.question(None, "Old index format found", "The index format can be upgraded to load articles quicker.\nUpgrading may take 1 to 5 minutes depending on the computer and language.\nDo you want to do this now ?", QtGui.QMessageBox.Yes | QtGui.QMessageBox.No) ==  QtGui.QMessageBox.No:
			outidxname_s = None
		else:
			try:
				QtGui.QMessageBox.information(None, "Indexing", "No window will be shown for the next few minutes") # warn that i'm too lazy to bg jobs GUI ;)
				convert_idx_s.build_sorted_entrylist(outidxname)
				os.utime(outblockname, None) # change atime to avoid false-positive above
				os.utime(outidxname, None)
			except AssertionError, e:
				QtGui.QMessageBox.critical(None, "Index postprocessing problem", "Sorry, the following error occured:\n%s\nThe program will now continue anyway in slow indexing mode" % unicode(e))
				outidxname_s = None
	
	return inputbz2Archive, outidxname, outblockname, outidxname_s



##
##	Main Wiki-syntax processing functions
##

def convertWikiList(txtLines):
	""" Parser for the namedlist/unnamedlist/definition """
	def indexDiff(a, b):
		x = 0
		for c1, c2 in zip(a, b):
			if c1 == c2:
				x += 1
			else:
				break
		return x

	out = ""
	mode = "%s"
	stack = []
	c = ""
	common = 0
	pattern = re.compile('[*#:;]+')
	for line in txtLines:
		linehead = re.match(pattern, line)
		if linehead:
			sl = linehead.end()
		sp = len(stack)
		common = indexDiff(stack, line)
		#for common, x in enumerate(zip(stack, line)):
		#	if x[0] != x[1]:
		#		break
		#else:
		#	common = min(len(stack), len(line))
		for x in range(sp, common, -1):
			c = stack.pop()
			#print "pop", c
			if c == '*':
				out += "</ul>"
			if c == '#':
				out += "</ol>"
			if c == ':':
				out += "</dd></dl>"
		lastpoped = c
		if not linehead:
			break
		for x in range(common, sl):
			c = line[x]
			stack.append(c)
			#print "push", c
			if c == '*':
				out += "<ul>"
				mode = "<li>%s</li>"
			elif c == '#':
				out += "<ol>"
				mode = "<li>%s</li>"
			elif c == ':' and lastpoped == '*':
				out += "<dl><dd>"
				mode = "<dd>%s</dd>"
			elif c == ';':
				k = line.find(':', x+1)
				if k > -1:
					head, line = line[x+1:k], line[k+1:]
					sl = 0
				else:
					head, line = line[x+1:], "" # "" will be skipped
				out += "<dl><dt><b>%s</b></dt><dd>" % head
				stack[-1] = ':'
				mode = "%s"
		k = line[sl:].strip()
		if k:
			out += mode % k
	return out

def filterMarkup(t, mathRendererInstance = None):
	"""Ref: http://meta.wikimedia.org/wiki/Help:Wikitext_reference"""
	t = re.subn('(?s)<!--.*?-->', "", t)[0] # force removing comments
	
	t = re.subn("(\n\[\[[a-z][a-z][\w-]*:[^:\]]+\]\])+$","", t)[0] # force remove last (=languages) list
	
	def equal2h(m):
		m = m.groupdict()
		m['level'] = str(len(m['level']))
		return "\n<h%(level)s>%(title)s</h%(level)s>\n<br/>\n" % m
	t = re.subn("\n(?P<level>=+) *(?P<title>[^\n]*)\\1 *(?=\n)", equal2h, t )[0]
	
	if mathRendererInstance: # was set to None if texvc is unavailable
		t = re.sub("(?s)<math>(.*?)</math>", mathRendererInstance.parse_exp, t)
	
	t = re.sub("'''(.+?)'''", "<b>\\1</b>", t)
	t = re.sub("''(.+?)''", "<i>\\1</i>", t)
	
	t = re.subn("(?u)^ \t]*==[ \t]*(\w)[ \t]*==[ \t]*\n", '<h2>(Image: \\1)</h2>', t)[0]
	# Instead of trying to implement a recursive parser which i want to avoid as much as possible,
	# simply do a 2-depth-only substitution with a 2-pass process
	t = re.subn("{{([^}{]*)}}", macroGeneric, t)[0]
	t = re.subn("{{([^}]*)}}", macroGeneric, t)[0]
	# FIXME : those patterns can't recurse (it _is_ REGex after all :-)
	# Therefore may fail on such cases as : [[Image:plop.png|This is an [[image]] with link]]
	# I don't think it's a big deal as for now
	
	t = re.subn("\[\[([^][|:]*)\]\]", '<a href="\\1">\\1</a>', t)[0]
	t = re.subn("\[\[([^]|[:]*)\|([^][]*)\]\]", '<a href="\\1">\\2</a>', t)[0]
	t = re.subn('\n----', '\n<hr/>', t)[0]
	def img2alt(m):
		imgname, other = m.groups()
		alttxt = other[other.rfind('|')+1:]
		return '<font color="#FF00CC"><i>(Image: %s, %s)</i></font>' % (imgname, alttxt)
	t = re.subn("\[\[[Ii]mage:([^.]*)(.*?)\]\]", img2alt, t)[0] # todo: parser l'interieur
	
	def wiki2table(match):
		m = match.groupdict()
		#print re.split('\n\|-+', m['body'])
		s = "<table %s>\n" % m['head']
		lines = []
		for T in re.split('\n\|-+', m['body']):
			if T.startswith('|-'):
				T = T[2:]
			T = re.subn('([|!])\\1', '\n\\1', T)[0]
			def tmp(m):
				a,b,c = m.groups()
				if a == '!':
					return "<th %s>%s</th>" % (b[:-1], c)
				else:
					return "<td %s>%s</td>" % (b[:-1], c)
			T = re.subn('([|!])((?:[^|\n]+\|)?)([^|\n]*)', tmp, T)[0]
			lines.append(T)
		s += "<tr>\n" + "</tr>\n<tr>".join(lines) + "</tr>\n"
		s += "</table>\n"
		if m['caption']:
			s += "Table Caption : %s<br/>\n" % m['caption'][2:]
		return s
	t = re.subn('\{\|(?P<head>[^!|}]+)(?P<caption>(\|\+.*)?)(?P<body>(.*\n)+?)\|\}', wiki2table, t)[0]
	
	t = re.subn("\n(([#*:;]+[^\n]+\n)+)", lambda m : convertWikiList(m.group().split('\n')[1:]), t)[0]
		
	t = re.sub("\n\n+", "\n<br/>", t)
	t = re.sub("(<br/>\n?)+", "\n<br/>", t) # final cleanup :-)
	
	footnotes = []
	namedref = {}
	def footparse(m):
		op, cl = m.groups()
		if op.startswith('>'):	# regular ref
			footnotes.append(op[1:])
			return "<sup>[%d]</sup>" % len(footnotes)
		elif op.startswith(' name'): # named ref
			key = op.lstrip()[op.find('"'):op.find('"',8)]
			if not namedref.has_key(key):
				footnotes.append("")
				namedref[key] = len(footnotes)
			if cl != '/>':
				footnotes[namedref[key] - 1] = op[op.find('>')+1:]
			return "<sup>[%d]</sup>" % namedref[key]
	def footwrite(footnotes):
		if footnotes == []:
			return ""
		else:
			return "<br/>".join(["<b>%d.</b> %s\n" % (i+1, x) for i, x in enumerate(footnotes)])
	# FIXME : footnotes may not go to the '<references/>' tag depending on the macros and languages
	t = re.subn('(?s)<ref([> ].*?)(</ref>|/>)', footparse, t)[0]
	t, hasfound = re.subn('<references/>', footwrite(footnotes), t)
	if (not hasfound) and len(footnotes):
		t = t + "<hr><h2><i>Notes</i></h2>" + footwrite(footnotes)
	return t

###
###	Macro-specific parsing
###
# Macro content may introduces "\n" which conflicts with list-parsing code, thus s/\n/ /
def macroGeneric(mo):
	argv = mo.group()[2:-2].replace('\n', '').split('|')
	# Only one macro example implemented so far :-)
	if argv[0].startswith('formatnum:'):
		t = argv[0][10:]
	#elif argv[0].startswith(....): t = ...
	elif argv[0].startswith('main'):
		txt = "(main article : [[%s]])" % argv[1]
		t = '<i><font color="#FF4400">%s</font></i><br/>' % txt
	elif argv[0].startswith('reflist'):
		t = "<references/>"
	else:
		txt = "<b> %s</b> %s " % (argv[0], ", ".join(argv[1:]))
		t = '<i><font color="#FF4400">%s</font></i>' % txt
	return t


###
###	Init code
###

def main():
	app = QtGui.QApplication(sys.argv)
	w = MainViewer()
	w.setWindowFlags(Qt.Window)
	w.show()
	sys.exit(app.exec_())

try: # Test if i'm running in IPython, for easy debugging. 
	# If so, i'll use the IPython's background Qt-loop feature
	print _ip
	print "Running in ipython mode "
	if  QtGui.qApp.instance() == None:
		app = QtGui.QApplication(sys.argv)
		w = MainViewer()
		w.setWindowFlags(Qt.WindowMinMaxButtonsHint)
		w.show()
except NameError:
	main()
