# coding=UTF-8
from gettext import gettext as _
import gtk
import glipper


def on_email(about, mail):
	gtk.show_uri(None, "mailto:%s" % mail, gtk.gdk.CURRENT_TIME)

def on_url(about, link):
	gtk.show_uri(None, link, gtk.gdk.CURRENT_TIME)

gtk.about_dialog_set_email_hook(on_email)
gtk.about_dialog_set_url_hook(on_url)

class About(object):
	__instance = None
	
	def __init__(self):
		if About.__instance == None:
			About.__instance = self
		else:
			About.__instance.about.present()
			return
			
		self.about = gtk.AboutDialog()
	
		infos = {
			"name" : _("Glipper"),
			"logo-icon-name" : "glipper",
			"version" : glipper.VERSION,
			"comments" : _("A clipboard manager."),
			"copyright" : "Copyright © 2007 Sven Rech, Eugenio Depalo, Karderio.\nCopyright © 2011 Laszlo Pandy",
			"website" : "http://launchpad.net/glipper",
			"website-label" : _("Glipper website"),
		}

		#about.set_artists([])
		#about.set_documenters([])
		self.about.set_authors(["Sven Rech <sven@gmx.de>",
		                        "Eugenio Depalo <eugeniodepalo@mac.com>",
		                        "Karderio <karderio@gmail.com>",
		                        "Laszlo Pandy <laszlok2@gmail.com>"])
	
		#translators: These appear in the About dialog, usual format applies.
		self.about.set_translator_credits( _("translator-credits") )
	
		for prop, val in infos.items():
			self.about.set_property(prop, val)
	
		self.about.connect("response", self.destroy)
		self.about.show_all()
		
	def destroy(self, dialog, response):
		dialog.destroy()
		About.__instance = None
