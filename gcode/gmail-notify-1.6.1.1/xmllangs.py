#!/usr/bin/python
# -*- coding: utf-8 -*-

# xmllangs 0.0.1
#
# HOW TO USE:
# 1) Create an instance of 'LangsParser' class. The only argument
#    its constructor takes is the filename of the language xml file.
# 2) To find a language use the 'find_lang' method of the object
#    created above. This method returns a 'Lang' class object.
# 3) To get all the languages readed from the XML file, use the
#    'get_all_langs' method from the 'LangsParser' class object.
# 4) To get a specific string from a 'Lang' class object, use the
#    'get_string' method. It takes only one parameter: the 'id' of
#    the string.
# 5) To get the name of a language from a 'Lang' class object use
#    the 'get_name' method. It returns a string.

# Language XML file structure
#
# <?xml version="1.0" encoding="utf-8" ?>
# <langs>
#     <lang name="myLanguage">
#         <string id="1">Here goes my first string</string>
#         <string id="2">This is my second string</string>
#         ...
#     </lang>
#     ...
# </langs>
#
# by Juan Grande
# juan.grande@gmail.com

from xml.sax.handler import ContentHandler
from xml import sax

# A string description
class String:
	def __init__( self, id):
		self.id = id
		self.string=str()

	# Accessors an mutators
	def set_string(self, string):
		self.string = string

	def get_string( self):
		return self.string

	def set_id( self, id):
		self.id = id

	def get_id(self):
		return self.id

# A language description
class Lang:
	def __init__(self, name):
		self.name=name
		self.strings=list()

	# Adds a string to this language
	def add_string(self, string):
		self.strings.append( string)
	# Returns a string with the name of the language
	def get_name(self):
		return self.name
	# Returns the string with id=='id'
	def get_string(self, id):
		for s in self.strings:
			if s.id==str(id):
				return s.get_string()
		return "(empty)"

# Language XML file parser
class LangHandler(ContentHandler):
	def __init__(self):
		self.langs=list()
	def startElement( self, name, attrs):
		if name=="lang":
			temp_lang = Lang(attrs.getValue("name")) 
			self.langs.append(temp_lang)
		if name=="string":
			s = String( attrs.getValue("id"))
			temp_lang = self.langs.pop()
			temp_lang.add_string(s)
			self.langs.append( temp_lang)
	
	def endElement( self, name):
		return

	def characters( self, content):
		if content.strip()!="":
			l = self.langs.pop()
			s = l.strings.pop()
			s.set_string(content)
			l.strings.append(s)
			self.langs.append(l)

# The main class
class LangsParser:
	def __init__(self, filename):
		self.lh = LangHandler()
		try:
			p = sax.parse( filename, self.lh)
			print "xmllangs: XML file succesfully parsed"
		except:
			print "xmllangs: Error parsing XML file."
	# Returns a list of all 'Lang' objects
	def get_all_langs( self):
		return self.lh.langs
	
	# Returns a 'Lang' class object where Lang.name==langname
	def find_lang(self, langname):
		for one_lang in self.lh.langs:
			if one_lang.get_name()==langname:
				return one_lang
		return self.lh.langs[0]
