#!/usr/bin/env python
# -*- coding: utf-8 -*-
import pygtk
pygtk.require('2.0')
import os
import gtk
import ConfigParser
import gmailatom
import xmllangs
import sys

LANGSXML_PATH=sys.path[0]+"/langs.xml"
ICON_PATH=sys.path[0]+"/gmail-notify-icon.png"

class GmailConfigWindow:

	# Public variables
	configElements = None 

	# Declare global variables for configuration as dictionary
	options = { "gmailusername":None, "gmailpassword":None, "browserpath":"www-browser", "proxy":None,
				"lang":"English", "voffset":0, "hoffset":0, "checkinterval":20000, 
				"animationdelay":15, "popuptimespan":5000}
					
	config = ConfigParser.RawConfigParser()
	loadedConfig = ""
	readLoginFromFile = None

	langs_parser=None
	
	def __init__( self):
		# Read configuration
		self.readConfig()

		self.window = gtk.Window( gtk.WINDOW_TOPLEVEL )
		self.window.set_title( self.lang.get_string(1) )
		self.window.set_border_width( 5 )
		self.window.set_position( gtk.WIN_POS_CENTER )
		self.window.set_modal( gtk.TRUE )
		self.window.set_resizable(gtk.FALSE)
		icon = gtk.gdk.pixbuf_new_from_file(ICON_PATH)
		gtk.window_set_default_icon_list( (icon) )

		# Register events
		self.window.connect( "delete_event", self.onDelete )
		
		# configElements = [ [Option Name, String ID, Entry, Label ], ...  ]
		self.configElements =	[
						["gmailusername",2,None,None],
						["gmailpassword",22,None,None],
						["browserpath",3,None,None],
						["proxy",36,None,None],
						["voffset",28,None,None],
						["hoffset",27,None,None],
						["checkinterval",31,None,None],
						["animationdelay",29,None,None],
						["popuptimespan",30,None,None]
					]

		# Create table and attach to window
		table = gtk.Table( rows=12, columns=2, homogeneous=gtk.FALSE )

		self.window.add(table)

		# Create and attach widgets
		for i in range( len(self.configElements) ):
			curVar = self.configElements[i][0]
			curLabel = self.configElements[i][1]
			
			label 	= gtk.Label( self.lang.get_string(curLabel) )
			label.set_alignment(0, 0.5)
			textbox	= gtk.Entry( max=0 )
			
			if ( self.options[curVar] != None ): 
				textbox.set_text( str( self.options[curVar] ) )

			if ( curVar == "gmailpassword" ):
				textbox.set_visibility( gtk.FALSE )
				textbox.set_invisible_char( '*' )

			# Store widget in element array
			self.configElements[i][2] = textbox
			self.configElements[i][3] = label
			table.attach(
							label,
							0, 1, i, i+1,
							xpadding=2,
							ypadding=1
						)

			table.attach(
							textbox,
							1,2, i, i+1,
							xpadding=2,
							ypadding=1
						)

			label.show()
			textbox.show()

		# Add checkbox to save username/pass to file
		alignment = gtk.Alignment( 0.5, 0.5, 0.0, 0.0 )
		self.savePassword = gtk.CheckButton( label=self.lang.get_string(34) )
		alignment.add( self.savePassword )

		if ( self.readLoginFromFile == None and ( self.options["gmailusername"] != None and self.options["gmailpassword"] != None ) ):
			self.readLoginFromFile = True
		else:
			self.readLoginFromFile = False
		

		if ( self.readLoginFromFile ):
			self.savePassword.set_active( gtk.TRUE )
		else:
			self.savePassword.set_active( gtk.FALSE )
			
		self.savePassword.show()
		table.attach( alignment, 0, 2, 10, 11 )
		alignment.show()

		# Add combobox to select language 
		self.lbl_langs=gtk.Label(self.lang.get_string(4))
		self.lbl_langs.set_alignment(0, 0.5)
		self.cbo_langs=gtk.combo_box_new_text()
		self.cbo_langs.connect( 'changed', self.update_labels)
                for one_lang in self.langs:
                        if one_lang==self.lang:
                                self.cbo_langs.prepend_text( one_lang.get_name())
                        else:
                                self.cbo_langs.append_text( one_lang.get_name())
		self.cbo_langs.set_active(0)
		# Attach combobox and label
		table.attach( self.lbl_langs, 0, 1, 9, 10 )
		self.lbl_langs.show()
		table.attach( self.cbo_langs, 1, 2, 9, 10, xpadding=5, ypadding=5 )
		self.cbo_langs.show()
		
		# Add 'Close' button
		button = gtk.Button( stock=gtk.STOCK_OK )
		table.attach( button, 0, 2, 11, 12, xpadding=2, ypadding=2 )
		button.connect( "clicked", self.onOkay )
		button.show()

		table.show()

	def show(self):
		self.window.show()
		self.main()
		
	def hide(self):
		self.window.hide()

	def checkfile(self,path):
		try:
			return open(path)
		except IOError:
			return None
		
	def readConfig( self ):
		self.config = ConfigParser.RawConfigParser()
		readFiles = self.config.read( [os.path.expanduser( "~/.notifier.conf" ), sys.path[0]+"/notifier.conf", "/etc/notifier.conf"] )

		#check readFiles has len, if not the next if will fail so use my method
		#to check if any of the configs exist and set self.loadedConfig appropriately
		#len(readFiles) always fails on my (John's) system so maybe it fails on others
		self.lenfail=0
		try:
			len( readFiles )
		except(TypeError):
			self.lenfail=1
			if (self.checkfile("/etc/notifier.conf")):
				self.loadedConfig="/etc/notifier.conf"
			elif (self.checkfile(sys.path[0]+"/notifier.conf")):
				self.loadedConfig=sys.path[0]+"/notifier.conf"
			elif (self.checkfile(os.path.expanduser("~/.notifier.conf"))):
				self.loadedConfig=os.path.expanduser("~/.notifier.conf")
			else:
				self.loadedConfig = os.path.expanduser("~/.notifier.conf")
				self.config.add_section("options")					

		# Save the file it read configuration options from
		if self.lenfail==0:
			if ( len( readFiles ) <= 0 ):
				self.loadedConfig = os.path.expanduser( "~/.notifier.conf" )
				self.config.add_section( "options" )
			else:
				self.loadedConfig = readFiles[0]
		
		# Check which options are defined and override defaults
		for key in self.options.keys():
			if ( self.config.has_option( 'options', key ) ):
				if ( type(self.options[key]) == int ):
					self.options[key] = self.config.getint( 'options', key )
				else:
					self.options[key] = self.config.get( 'options', key ).replace( '"', '' )

		# Create langs and lang objects
		self.langs_parser = xmllangs.LangsParser(LANGSXML_PATH)
		self.langs = self.langs_parser.get_all_langs()
		self.lang = self.langs_parser.find_lang( self.options["lang"])
	
		print "Configuration read (" + self.loadedConfig + ")"
			
	def getOptions( self ):
		return self.options

	def onDelete( self, widget, data=None ):
		gtk.main_quit()
		self.hide()
		return gtk.TRUE
	
	def onOkay( self, widget, callback_data=None ):
		errorString = ""
		
		# Apply changes to options dictionary
		for curElement in self.configElements:
			if ( type(self.options[curElement[0]]) == int ):
				self.options[curElement[0]] = int(curElement[2].get_text())
			else:
				self.options[curElement[0]] = curElement[2].get_text()
		iter = self.cbo_langs.get_active_iter()
		self.options["lang"]=self.cbo_langs.get_model().get_value(iter, 0)	

		# Before writing, check for bad values
		try:
			tempLogin = gmailatom.GmailAtom( self.options["gmailusername"], self.options["gmailpassword"], self.options["proxy"])
			tempLogin.refreshInfo()
		except:
			print "Unexpected error:", sys.exc_info()[0]
			errorString = errorString + "Login appears to be invalid\n"

		if ( len( errorString ) == 0 ):
			# No errors, so write to config file
			for key in self.options.keys():
				self.config.set( "options", key, self.options[key] )

			if ( not self.savePassword.get_active() ):
				self.config.remove_option( "options", "gmailusername" )
				self.config.remove_option( "options", "gmailpassword" )

			fd = open( self.loadedConfig, 'w' )
			os.chmod( self.loadedConfig, 0600)
			self.config.write( fd )
			fd.close()
			gtk.main_quit()
			self.hide()
		else:
			dialog = gtk.MessageDialog( buttons=gtk.BUTTONS_OK, type=gtk.MESSAGE_ERROR )
			dialog.set_position( gtk.WIN_POS_CENTER )
			dialog.set_markup( errorString )
			dialog.run();
			dialog.destroy()

	def update_labels( self, combobox=None ):
		iter = self.cbo_langs.get_active_iter()
		lang_name=self.cbo_langs.get_model().get_value(iter, 0)	
		self.lang=self.langs_parser.find_lang(lang_name)
		for i in range(len(self.configElements)):
			self.configElements[i][3].set_label(self.lang.get_string(self.configElements[i][1]))
		self.lbl_langs.set_label(self.lang.get_string(4))
		self.savePassword.set_label(self.lang.get_string(34))
		self.window.set_title(self.lang.get_string(1))

	def get_lang( self ):
		return self.lang
			
	def no_username_or_password( self ):
		return ( self.options["gmailusername"] == None or self.options["gmailpassword"] == None )
			
	def main( self ):
		gtk.main()
		
		
if __name__ == "__main__":
	config = GmailConfigWindow()
	config.update_labels();
	config.show()
	#config.main()

