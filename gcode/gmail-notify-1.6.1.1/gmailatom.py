#!/usr/bin/python
# -*- coding: utf-8 -*-

# gmailatom 0.0.1
#
# HOW TO USE:
# 1) Create an instance of 'GmailAtom' class. The two arguments
#    its constructor take are the username (including '@gmail.com')
#    and the password.
# 2) To retrieve the account status call 'refreshInfo()'.
# 3) To get the unread messages count call 'getUnreadMsgCount()'.
#    You MUST call 'refreshInfo()' at least one time before using
#    this method or it will return zero.
# 4) To get specific information about an unread email you must
#    call the corresponding getter method passing to it the number
#    of the message. The number zero represents the newest one.
#    You MUST call 'refreshInfo()' at least one time before using any
#    getter method or they will return an empty string.
#    The getter methods are:
#	getMsgTitle(index)
#	getMsgSummary(index)
#	getMsgAuthorName(index)
#	getMsgAuthorEmail(index)
#
# by Juan Grande
# juan.grande@gmail.com

from xml.sax.handler import ContentHandler
from xml import sax
import urllib2
import os

# Auxiliar structure
class Mail:
	title=""
	summary=""
	author_name=""
	author_addr=""

# Sax XML Handler
class MailHandler(ContentHandler):
	
	# Tags
	TAG_FEED = "feed"
	TAG_FULLCOUNT = "fullcount"
	TAG_ENTRY = "entry"
	TAG_TITLE = "title"
	TAG_SUMMARY = "summary"
	TAG_AUTHOR = "author"
	TAG_NAME = "name"
	TAG_EMAIL = "email"
	
	# Path the information
	PATH_FULLCOUNT = [ TAG_FEED, TAG_FULLCOUNT ]
	PATH_TITLE = [ TAG_FEED, TAG_ENTRY, TAG_TITLE ]
	PATH_SUMMARY = [ TAG_FEED, TAG_ENTRY, TAG_SUMMARY ]
	PATH_AUTHOR_NAME = [ TAG_FEED, TAG_ENTRY, TAG_AUTHOR, TAG_NAME ]
	PATH_AUTHOR_EMAIL = [ TAG_FEED, TAG_ENTRY, TAG_AUTHOR, TAG_EMAIL ]

	def __init__(self):
		self.startDocument()

	def startDocument(self):
		self.entries=list()
		self.actual=list()
		self.mail_count="0"

	def startElement( self, name, attrs):
		# update actual path
		self.actual.append(name)

		# add a new email to the list
		if name=="entry":
			m = Mail()
			self.entries.append(m)

	def endElement( self, name):
		# update actual path
		self.actual.pop()

	def characters( self, content):
		# New messages count
		if (self.actual==self.PATH_FULLCOUNT):
			self.mail_count = self.mail_count+content

		# Message title
		if (self.actual==self.PATH_TITLE):
			temp_mail=self.entries.pop()
			temp_mail.title=temp_mail.title+content
			self.entries.append(temp_mail)

		# Message summary
		if (self.actual==self.PATH_SUMMARY):
			temp_mail=self.entries.pop()
			temp_mail.summary=temp_mail.summary+content
			self.entries.append(temp_mail)

		# Message author name
		if (self.actual==self.PATH_AUTHOR_NAME):
			temp_mail=self.entries.pop()
			temp_mail.author_name=temp_mail.author_name+content
			self.entries.append(temp_mail)

		# Message author email
		if (self.actual==self.PATH_AUTHOR_EMAIL):
			temp_mail=self.entries.pop()
			temp_mail.author_addr=temp_mail.author_addr+content
			self.entries.append(temp_mail)

	def getUnreadMsgCount(self):
		return int(self.mail_count)

# The mail class
class GmailAtom:

	realm = "New mail feed"
	host = "https://mail.google.com"
	url = host + "/mail/feed/atom" + (os.environ['GA_LABEL'] if 'GA_LABEL' in os.environ else '')

	def __init__(self, user, pswd, proxy=None):
		self.m = MailHandler()
		self.user = user
		self.pswd = pswd
		self.proxy = proxy

	def sendRequest(self):
		# initialize authorization handler
		
		auth_handler = urllib2.HTTPBasicAuthHandler()
		auth_handler.add_password( self.realm, self.host, self.user, self.pswd)
		proxy = self.proxy
		# manage proxy
		if proxy:
			proxy_handler = urllib2.ProxyHandler({'http': proxy})
			opener = urllib2.build_opener(proxy_handler, auth_handler)
		else:
			opener = urllib2.build_opener(auth_handler)
		urllib2.install_opener(opener)

		return urllib2.urlopen(self.url)

	def refreshInfo(self):
		# get the page and parse it
		p = sax.parseString( self.sendRequest().read(), self.m)

	def getUnreadMsgCount(self):
		return self.m.getUnreadMsgCount()

	def getMsgTitle(self, index):
		return self.m.entries[index].title

	def getMsgSummary(self, index):
		return self.m.entries[index].summary

	def getMsgAuthorName(self, index):
		return self.m.entries[index].author_name

	def getMsgAuthorEmail(self, index):
		return self.m.entries[index].author_email
