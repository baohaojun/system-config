import gtk, gio, glipper, gobject, gconf
from gettext import gettext as _
from os.path import *
from glipper.Plugin import *

class PluginsManager(gobject.GObject):
	__gsignals__ = {
		"menu-items-changed" : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE, []),
	}

	def __init__(self):
		gobject.GObject.__init__(self)
		self.plugins = [] # list of enabled plugins
		self.menu_items = [] # History menu items for plugins
		self.plugin_cache = None
		
		self.autostart_plugins = glipper.GCONF_CLIENT.get_list(glipper.GCONF_AUTOSTART_PLUGINS, gconf.VALUE_STRING)
		
		if self.autostart_plugins == None:
			self.autostart_plugins = ['nopaste']
		glipper.GCONF_CLIENT.notify_add(glipper.GCONF_AUTOSTART_PLUGINS, lambda x, y, z, a: self.on_autostart_plugins_changed (z.value))

	def on_autostart_plugins_changed(self, value):
		if value is None or value.type != gconf.VALUE_LIST:
			return
		self.autostart_plugins = glipper.GCONF_CLIENT.get_list(glipper.GCONF_AUTOSTART_PLUGINS, gconf.VALUE_STRING)

	def load_cache(self):
		if self.plugin_cache is not None:
			return
		self.plugin_cache = {}
		
		directory = gio.File(path=glipper.PLUGINS_DIR)
		
		for file_info in directory.enumerate_children(gio.FILE_ATTRIBUTE_STANDARD_NAME):
			file_name = file_info.get_name()
			if file_name[-3:] == ".py":
				try:
					plugin = Plugin(file_name[:-3])
					self.plugin_cache[plugin.get_file_name()] = plugin
				except:
					print "Error trying to open plugin %s" % file_name
	
	def load(self):
		self.load_cache()

		for file_name in self.autostart_plugins:
			self.start(file_name)

	def start(self, file_name):
		if file_name in self.plugin_cache:
			plugin = self.plugin_cache[file_name]
			self.plugins.append(plugin)
			self.init_plugin = plugin # Adding menu items from plugins is allowed only in the init function.
				                      # add_menu_item uses this to know what plugin it is being called for.
			plugin.call('init')
			self.init_plugin = None

	def stop(self, file_name):
		for plugin in self.plugins:
			if plugin.get_file_name() == file_name:
				plugin.call('stop')
				self.plugins.remove(plugin)
				self.remove_menu_items(plugin)
				
	def stop_all(self):
		for plugin in self.plugins:
			self.stop(plugin)
	
	def get_menu_items(self):
		return self.menu_items
	
	def remove_menu_items(self, plugin):
		for p, menu_item in self.menu_items:
			if p == plugin:
				self.menu_items.remove((p, menu_item))

		self.emit('menu-items-changed')
	
	def add_menu_item(self, menu_item):
		self.menu_items.append((self.init_plugin, menu_item))
		self.emit('menu-items-changed')
		
	def get_started(self, file_name):
		for plugin in self.plugins:
			if plugin.get_file_name() == file_name:
				return True
		return False
		
	def get_autostarted(self, file_name):
		return file_name in self.autostart_plugins

	def call(self, signal, *args):
		for plugin in self.plugins:
			plugin.call(signal, *args)

class PluginsWindow(object):
	__instance = None

	def __init__(self):
		if PluginsWindow.__instance == None:
			PluginsWindow.__instance = self
		else:
			PluginsWindow.__instance.plugins_window.present()
			return
			
		builder_file = gtk.Builder()
		builder_file.add_from_file(join(glipper.SHARED_DATA_DIR, "plugins-window.ui"))
		
		self.FILE_NAME_COLUMN, self.ENABLED_COLUMN, self.AUTOSTART_COLUMN, self.NAME_COLUMN, self.DESCRIPTION_COLUMN, self.PREFERENCES_COLUMN = range(6)
		
		self.plugins_window = builder_file.get_object("plugins_window")
		self.plugins_list = builder_file.get_object("plugins_list")
		self.preferences_button = builder_file.get_object("preferences_button")
		self.refresh_button = builder_file.get_object("refresh_button")
		self.plugins_list_model = gtk.ListStore(gobject.TYPE_STRING, gobject.TYPE_BOOLEAN, gobject.TYPE_BOOLEAN, gobject.TYPE_STRING, gobject.TYPE_STRING, gobject.TYPE_BOOLEAN)
		self.plugins_list.set_model(self.plugins_list_model)
		self.plugins_list.get_selection().connect('changed', self.on_plugins_list_selection_changed)
		
		renderer = gtk.CellRendererToggle()
		renderer.connect('toggled', self.on_enabled_toggled)
		self.plugins_list.append_column(gtk.TreeViewColumn(_("Enabled"), renderer, active = self.ENABLED_COLUMN))
		renderer = gtk.CellRendererToggle()
		renderer.connect('toggled', self.on_autostart_toggled)
		self.plugins_list.append_column(gtk.TreeViewColumn(_("Autostart"), renderer, active = self.AUTOSTART_COLUMN))
		self.plugins_list.append_column(gtk.TreeViewColumn(_("Name"), gtk.CellRendererText(), text = self.NAME_COLUMN))
		self.plugins_list.append_column(gtk.TreeViewColumn(_("Description"), gtk.CellRendererText(), text = self.DESCRIPTION_COLUMN))
		
		self.autostart_plugins = glipper.GCONF_CLIENT.get_list(glipper.GCONF_AUTOSTART_PLUGINS, gconf.VALUE_STRING)
		if self.autostart_plugins == None:
			self.autostart_plugins = ['nopaste']
		self.autostart_plugins_notify = glipper.GCONF_CLIENT.notify_add(glipper.GCONF_AUTOSTART_PLUGINS, lambda x, y, z, a: self.on_autostart_plugins_changed (z.value))
		
		builder_file.connect_signals({
			'on_plugins_window_response': self.on_plugins_window_response,
			'on_preferences_button_clicked': self.on_preferences_button_clicked,
		})
		
		self.update_plugins_list_model()
		
		self.plugins_window.show_all()

	def update_plugins_list_model(self):
		self.plugins_list_model.clear()
		
		for plugin in get_glipper_plugin_cache():
			self.plugins_list_model.append([plugin.get_file_name(),
			                                plugins_manager.get_started(plugin.get_file_name()),
			                                plugins_manager.get_autostarted(plugin.get_file_name()),
			                                plugin.get_name(),
			                                plugin.get_description(),
			                                plugin.has_preferences()])

	def on_autostart_plugins_changed(self, value):
		if value is None or value.type != gconf.VALUE_LIST:
			return
		self.autostart_plugins = glipper.GCONF_CLIENT.get_list(glipper.GCONF_AUTOSTART_PLUGINS, gconf.VALUE_STRING)

		iter = self.plugins_list_model.get_iter_first()
		
		while iter:
			file_name = self.plugins_list_model.get_value(iter, self.FILE_NAME_COLUMN)
			
			if file_name in self.autostart_plugins:
				self.plugins_list_model.set_value(iter, self.AUTOSTART_COLUMN, True)
			else:
				self.plugins_list_model.set_value(iter, self.AUTOSTART_COLUMN, False)
				
			iter = self.plugins_list_model.iter_next(iter)
				
	def on_plugins_window_response(self, dialog, response):
		if response == gtk.RESPONSE_DELETE_EVENT or response == gtk.RESPONSE_CLOSE:
			dialog.destroy()
			glipper.GCONF_CLIENT.notify_remove(self.autostart_plugins_notify)
			
			PluginsWindow.__instance = None
		elif response == gtk.RESPONSE_HELP:
			gtk.show_uri(None, 'ghelp:glipper?plugins', gtk.gdk.CURRENT_TIME)
	
	def on_preferences_button_clicked(self, widget):
		treeview, iter = self.plugins_list.get_selection().get_selected()
		
		file_name = self.plugins_list_model.get_value(iter, self.FILE_NAME_COLUMN)
	
		plugin = get_glipper_plugin_from_cache(file_name)
		plugin.call('on_show_preferences', self.plugins_window)
		
	def on_plugins_list_selection_changed(self, selection):
		treeview, iter = selection.get_selected()
		
		preferences = self.plugins_list_model.get_value(iter, self.PREFERENCES_COLUMN)
 
		self.preferences_button.set_sensitive(preferences)
		
	def on_autostart_toggled(self, renderer, path):
		iter = self.plugins_list_model.get_iter(path)
		file_name = self.plugins_list_model.get_value(iter, self.FILE_NAME_COLUMN)
		
		if plugins_manager.get_autostarted(file_name):
			self.autostart_plugins.remove(file_name)
		else:
			self.autostart_plugins.append(file_name)
		
		self.plugins_list_model.set_value(iter, self.AUTOSTART_COLUMN, plugins_manager.get_autostarted(file_name))
		glipper.GCONF_CLIENT.set_list(glipper.GCONF_AUTOSTART_PLUGINS, gconf.VALUE_STRING, self.autostart_plugins)
		
	def on_enabled_toggled(self, renderer, path):
		iter = self.plugins_list_model.get_iter(path)
		file_name = self.plugins_list_model.get_value(iter, self.FILE_NAME_COLUMN)
  
		if plugins_manager.get_started(file_name):
			plugins_manager.stop(file_name)
		else:
			plugins_manager.start(file_name)
		
		self.plugins_list_model.set_value(iter, self.ENABLED_COLUMN, plugins_manager.get_started(file_name))

plugins_manager = PluginsManager()

def get_glipper_plugins_manager():
	return plugins_manager
def get_glipper_plugin_cache():
	plugins_manager.load_cache()
	return plugins_manager.plugin_cache.values()
def get_glipper_plugin_from_cache(file_name):
	plugins_manager.load_cache()
	return plugins_manager.plugin_cache.get(file_name, None)
