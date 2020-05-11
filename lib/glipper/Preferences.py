import glipper, gtk, gconf
from os.path import join


class Preferences(object):
	__instance = None
	
	def __init__(self):
		if Preferences.__instance == None:
			Preferences.__instance = self
		else:
			Preferences.__instance.preferences_window.present()
			return
			
		builder_file = gtk.Builder()
		builder_file.add_from_file(join(glipper.SHARED_DATA_DIR, "preferences-window.ui"))
		
		self.preferences_window = builder_file.get_object("preferences_window")
		self.max_elements_spin = builder_file.get_object("max_elements_spin")
		self.use_primary_clipboard_check = builder_file.get_object("use_primary_clipboard_check")
		self.use_default_clipboard_check = builder_file.get_object("use_default_clipboard_check")
		self.mark_default_entry = builder_file.get_object("mark_default_entry")
		self.save_history_check = builder_file.get_object("save_history_check")
		self.key_combination_entry = builder_file.get_object("key_combination_entry")
	
		self.max_elements_spin.set_value(glipper.GCONF_CLIENT.get_int(glipper.GCONF_MAX_ELEMENTS))
		self.use_primary_clipboard_check.set_active(glipper.GCONF_CLIENT.get_bool(glipper.GCONF_USE_PRIMARY_CLIPBOARD))
		self.use_default_clipboard_check.set_active(glipper.GCONF_CLIENT.get_bool(glipper.GCONF_USE_DEFAULT_CLIPBOARD))
		self.use_primary_clipboard_check.set_active(glipper.GCONF_CLIENT.get_bool(glipper.GCONF_USE_PRIMARY_CLIPBOARD))
		self.mark_default_entry.set_active(glipper.GCONF_CLIENT.get_bool(glipper.GCONF_MARK_DEFAULT_ENTRY))
		self.save_history_check.set_active(glipper.GCONF_CLIENT.get_bool(glipper.GCONF_SAVE_HISTORY))

		key_combo = glipper.GCONF_CLIENT.get_string(glipper.GCONF_KEY_COMBINATION)
		if key_combo is None: key_combo = ''
		self.key_combination_entry.set_text(key_combo)
		
		self.max_elements_notify = glipper.GCONF_CLIENT.notify_add(glipper.GCONF_MAX_ELEMENTS, lambda x, y, z, a: self.on_max_elements_changed (z.value))
		self.use_primary_clipboard_notify = glipper.GCONF_CLIENT.notify_add(glipper.GCONF_USE_PRIMARY_CLIPBOARD, lambda x, y, z, a: self.on_use_primary_clipboard_changed (z.value))
		self.use_default_clipboard_notify = glipper.GCONF_CLIENT.notify_add(glipper.GCONF_USE_DEFAULT_CLIPBOARD, lambda x, y, z, a: self.on_use_default_clipboard_changed (z.value))
		self.mark_default_entry_notify = glipper.GCONF_CLIENT.notify_add(glipper.GCONF_MARK_DEFAULT_ENTRY, lambda x, y, z, a: self.on_mark_default_entry_changed (z.value))
		self.save_history_notify = glipper.GCONF_CLIENT.notify_add(glipper.GCONF_SAVE_HISTORY, lambda x, y, z, a: self.on_save_history_changed (z.value))
		self.key_combination_notify = glipper.GCONF_CLIENT.notify_add(glipper.GCONF_KEY_COMBINATION, lambda x, y, z, a: self.on_key_combination_changed (z.value))
		
		builder_file.connect_signals({
			'on_preferences_window_response': self.on_preferences_window_response,
			'on_max_elements_spin_value_changed': self.on_max_elements_spin_value_changed,
			'on_use_primary_clipboard_check_toggled': self.on_use_primary_clipboard_check_toggled,
			'on_use_default_clipboard_check_toggled': self.on_use_default_clipboard_check_toggled,
			'on_mark_default_entry_toggled': self.on_mark_default_entry_toggled,
			'on_save_history_check_toggled': self.on_save_history_check_toggled,
			'on_key_combination_entry_changed': self.on_key_combination_entry_changed,
			'on_help_button_clicked': self.on_help_button_clicked
		})
		
		self.update_mark_default_entry()
		
		self.preferences_window.show_all()
	
	def update_mark_default_entry(self):
		if self.use_default_clipboard_check.get_active():
			self.mark_default_entry.set_sensitive(True)
		else:
			self.mark_default_entry.set_sensitive(False)
	
	def on_max_elements_changed(self, value):
		if value is None or value.type != gconf.VALUE_INT:
			return
		self.max_elements_spin.set_value(value.get_int())
	
	def on_use_primary_clipboard_changed(self, value):
		if value is None or value.type != gconf.VALUE_BOOL:
			return
		self.use_primary_clipboard_check.set_active(value.get_bool())
	
	def on_use_default_clipboard_changed(self, value):
		if value is None or value.type != gconf.VALUE_BOOL:
			return
		self.use_default_clipboard_check.set_active(value.get_bool())
	
	def on_mark_default_entry_changed(self, value):
		if value is None or value.type != gconf.VALUE_BOOL:
			return
		self.mark_default_entry.set_active(value.get_bool())
	
	def on_save_history_changed(self, value):
		if value is None or value.type != gconf.VALUE_BOOL:
			return
		self.save_history_check.set_active(value.get_bool())
	
	def on_key_combination_changed(self, value):
		if value is None or value.type != gconf.VALUE_STRING:
			return
		self.key_combination_entry.set_text(value.get_string())
	
	def on_max_elements_spin_value_changed(self, spin):
		glipper.GCONF_CLIENT.set_int(glipper.GCONF_MAX_ELEMENTS, int(spin.get_value()))
		
	def on_use_primary_clipboard_check_toggled(self, toggle):
		glipper.GCONF_CLIENT.set_bool(glipper.GCONF_USE_PRIMARY_CLIPBOARD, toggle.get_active())
		
	def on_use_default_clipboard_check_toggled(self, toggle):
		self.update_mark_default_entry()
		glipper.GCONF_CLIENT.set_bool(glipper.GCONF_USE_DEFAULT_CLIPBOARD, toggle.get_active())
		
	def on_mark_default_entry_toggled(self, toggle):
		glipper.GCONF_CLIENT.set_bool(glipper.GCONF_MARK_DEFAULT_ENTRY, toggle.get_active())
		
	def on_save_history_check_toggled(self, toggle):
		glipper.GCONF_CLIENT.set_bool(glipper.GCONF_SAVE_HISTORY, toggle.get_active())
		
	def on_key_combination_entry_changed(self, entry):
		glipper.GCONF_CLIENT.set_string(glipper.GCONF_KEY_COMBINATION, entry.get_text())
		
	def on_preferences_window_response(self, dialog, response):
		if response == gtk.RESPONSE_DELETE_EVENT or response == gtk.RESPONSE_CLOSE:
			dialog.destroy()
			glipper.GCONF_CLIENT.notify_remove(self.max_elements_notify)
			glipper.GCONF_CLIENT.notify_remove(self.use_default_clipboard_notify)
			glipper.GCONF_CLIENT.notify_remove(self.use_primary_clipboard_notify)
			glipper.GCONF_CLIENT.notify_remove(self.mark_default_entry_notify)
			glipper.GCONF_CLIENT.notify_remove(self.save_history_notify)
			glipper.GCONF_CLIENT.notify_remove(self.key_combination_notify)
			Preferences.__instance = None
	
	def on_help_button_clicked(self, button):
		gtk.show_uri(None, 'ghelp:glipper?preferences', gtk.gdk.CURRENT_TIME)
