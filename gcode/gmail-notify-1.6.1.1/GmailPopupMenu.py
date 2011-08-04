#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Uploaded by juan_grande 2005/02/14 18:38 UTC
import pygtk
import gtk

class GmailPopupMenu:
	def __init__(self, gmailnotify):
		# Create menu items
		self.item_check = gtk.MenuItem( gmailnotify.lang.get_string(9), gtk.TRUE)
		self.item_usage = gtk.MenuItem( gmailnotify.lang.get_string(10), gtk.TRUE)
		self.item_inbox = gtk.MenuItem( gmailnotify.lang.get_string(23), gtk.TRUE)
		self.item_conf  = gtk.MenuItem( gmailnotify.lang.get_string(11), gtk.TRUE)
		self.item_exit = gtk.MenuItem( gmailnotify.lang.get_string(12), gtk.TRUE)
		# Connect the events
		self.item_check.connect( 'activate', gmailnotify.mail_check)
		self.item_usage.connect( 'activate', gmailnotify.show_quota_info)
               	self.item_inbox.connect( 'activate', gmailnotify.gotourl)
		self.item_conf.connect( 'activate', gmailnotify.update_config)
		self.item_exit.connect( 'activate', gmailnotify.exit)
		# Create the menu
		self.menu = gtk.Menu()
		# Append menu items to the menu
		self.menu.append( self.item_check)
		#self.menu.append( self.item_usage)
		self.menu.append( self.item_inbox)
		self.menu.append( gtk.SeparatorMenuItem())
		self.menu.append( self.item_conf)
		self.menu.append( gtk.SeparatorMenuItem())
		self.menu.append( self.item_exit)
		self.menu.show_all()

		return

	def show_menu(self, event):
		# Display the menu
		self.menu.popup( None, None, None, event.button, event.time)
		return
