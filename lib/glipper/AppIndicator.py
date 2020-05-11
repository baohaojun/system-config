import os
from os.path import *
import gtk, gconf, gobject
from gettext import gettext as _

import glipper, glipper.About, glipper.Preferences
from glipper.Keybinder import *
from glipper.History import *
from glipper.PluginsManager import *

class StatusIcon:
	def __init__(self):
		self.status_icon = gtk.StatusIcon()
		self.status_icon.set_from_icon_name("glipper")
		self.status_icon.set_visible(True)
		self.status_icon.connect('popup-menu', self.on_status_icon_popup_menu)
		self.status_icon.connect('button-press-event', self.on_status_icon_button_press)
		
	def on_status_icon_button_press(self, status_icon, event):
		self.menu.popup(None, None, gtk.status_icon_position_menu, event.button, event.time, status_icon)
	
	def on_status_icon_popup_menu(self, status_icon, button_num, activate_time):
		# this will call gtk.status_icon_position_menu(menu, status_icon) before displaying the menu
		self.menu.popup(None, None, gtk.status_icon_position_menu, button_num, activate_time, status_icon)
		
	def set_menu(self, menu):
		self.menu = menu

class AppIndicator(object):
	def __init__(self):
		self.menu = gtk.Menu()
		self._app_indicator = None
		self._status_icon = None
		
		try:
			import appindicator
		except ImportError:
			self._status_icon = StatusIcon()
			self._status_icon.set_menu(self.menu)
		else:
			self._app_indicator = appindicator.Indicator("glipper", "glipper", appindicator.CATEGORY_APPLICATION_STATUS)
			self._app_indicator.set_status(appindicator.STATUS_ACTIVE)
			self._app_indicator.set_menu(self.menu)

		glipper.GCONF_CLIENT.notify_add(glipper.GCONF_MARK_DEFAULT_ENTRY, lambda x, y, z, a: self.update_menu(get_glipper_history().get_history()))
		glipper.GCONF_CLIENT.notify_add(glipper.GCONF_MAX_ITEM_LENGTH, lambda x, y, z, a: self.update_menu(get_glipper_history().get_history()))
		
		gtk.window_set_default_icon_name("glipper")
		
		get_glipper_keybinder().connect('activated', self.on_key_combination_press)
		get_glipper_keybinder().connect('changed', self.on_key_combination_changed)
		get_glipper_history().connect('changed', self.on_history_changed)
		get_glipper_plugins_manager().load()
		get_glipper_history().load()
		get_glipper_plugins_manager().connect('menu-items-changed', self.on_plugins_menu_items_changed)

	def on_plugins_menu_items_changed(self, manager):
		self.update_menu(get_glipper_history().get_history())
	
	def on_menu_item_activate(self, menuitem, item):
		get_glipper_clipboards().set_text(item)
		
	def on_clear(self, menuitem):
		get_glipper_history().clear()
		get_glipper_clipboards().clear_text()
	
	def update_menu(self, history):
		plugins_menu_items = get_glipper_plugins_manager().get_menu_items()
		
		for c in self.menu.get_children():
			self.menu.remove(c)
			
		if len(history) == 0:
			menu_item = gtk.ImageMenuItem(gtk.STOCK_STOP)
			menu_item.get_child().set_markup(_('<i>Empty history</i>'))
			menu_item.set_sensitive(False)
			self.menu.append(menu_item)
		else:
			for item in history:
				menu_item = gtk.CheckMenuItem(format_item(item), False)
				menu_item.set_property('draw-as-radio', True)

				if len(item) > max_item_length :
					menu_item.set_tooltip_text(item[:glipper.MAX_TOOLTIPS_LENGTH])

				if mark_default_entry and item == get_glipper_clipboards().get_default_clipboard_text():
					menu_item.get_child().set_markup('<b>' + gobject.markup_escape_text(menu_item.get_child().get_text()) + '</b>')
					menu_item.set_property('active', True)
				else:
					menu_item.set_property('active', False)
					
				menu_item.connect('activate', self.on_menu_item_activate, item)
				self.menu.append(menu_item)
		
		self.menu.append(gtk.SeparatorMenuItem())
		
		clear_item = gtk.ImageMenuItem(gtk.STOCK_CLEAR)
		clear_item.connect('activate', self.on_clear)
		self.menu.append(clear_item)
		
		if len(plugins_menu_items) > 0:
			self.menu.append(gtk.SeparatorMenuItem())
			
			for module, menu_item in plugins_menu_items:
				self.menu.append(menu_item)
				
		preferences_item = gtk.ImageMenuItem(gtk.STOCK_PREFERENCES)
		preferences_item.connect('activate', self.on_preferences)
		help_item = gtk.ImageMenuItem(gtk.STOCK_HELP)
		help_item.connect('activate', self.on_help)
		about_item = gtk.ImageMenuItem(gtk.STOCK_ABOUT)
		about_item.connect('activate', self.on_about)
		plugins_item = gtk.MenuItem(_("Pl_ugins"), True)
		plugins_item.connect('activate', self.on_plugins)
		
		self.menu.append(gtk.SeparatorMenuItem())
		self.menu.append(preferences_item)
		# uncomment when installing of help files works correctly
		#self.menu.append(help_item)
		self.menu.append(about_item)
		self.menu.append(plugins_item)
		
		self.menu.show_all()

	def on_preferences (self, component):
		glipper.Preferences.Preferences()
		
	def on_help (self, component):
		gtk.show_uri(None, 'ghelp:glipper', gtk.gdk.CURRENT_TIME)
	
	def on_about (self, component):
		glipper.About.About()
	
	def on_plugins (self, component):
		PluginsWindow()
	
	def on_key_combination_press(self, widget, time):
		self.menu.popup(None, None, None, 1, gtk.get_current_event_time())
	
	def on_key_combination_changed(self, keybinder, success):
		if success:
			pass # update key combination tooltip when applicable
			
	def on_history_changed(self, history, history_list):
		self.update_menu(history_list)
		get_glipper_plugins_manager().call('on_history_changed')
		if save_history:
			history.save()

# These variables and functions are available for all Applet instances:

mark_default_entry = glipper.GCONF_CLIENT.get_bool(glipper.GCONF_MARK_DEFAULT_ENTRY)
if mark_default_entry == None:
	mark_default_entry = True
glipper.GCONF_CLIENT.notify_add(glipper.GCONF_MARK_DEFAULT_ENTRY, lambda x, y, z, a: on_mark_default_entry_changed (z.value))

save_history = glipper.GCONF_CLIENT.get_bool(glipper.GCONF_SAVE_HISTORY)
if save_history == None:
	save_history = True
glipper.GCONF_CLIENT.notify_add(glipper.GCONF_SAVE_HISTORY, lambda x, y, z, a: on_save_history_changed (z.value))

max_item_length = glipper.GCONF_CLIENT.get_int(glipper.GCONF_MAX_ITEM_LENGTH)
if max_item_length == None:
	max_elements = 35
glipper.GCONF_CLIENT.notify_add(glipper.GCONF_MAX_ITEM_LENGTH, lambda x, y, z, a: on_max_item_length_changed (z.value))

def on_mark_default_entry_changed(value):
	global mark_default_entry
	if value is None or value.type != gconf.VALUE_BOOL:
		return
	mark_default_entry = value.get_bool()

def on_save_history_changed(value):
	global save_history
	if value is None or value.type != gconf.VALUE_BOOL:
		return
	save_history = value.get_bool()

def on_max_item_length_changed (value):
	global max_item_length
	if value is None or value.type != gconf.VALUE_INT:
		return
	max_item_length = value.get_int()

def format_item(item):
	i = item.replace("\n", " ")
	i = i.replace("\t", " ")
	if len(item) > max_item_length:
	  return i[0:max_item_length/2] + u'\u2026' + i[-(max_item_length/2-3):]
	return i
