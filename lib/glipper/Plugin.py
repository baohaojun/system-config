import sys

class Plugin(object):
	def __init__(self, file_name):
		self.file_name = file_name
		self.load_module()
		self.info = self.call('info')
	
	def load_module(self):
		try:
			self.module = __import__(self.file_name)
		except ImportError:
			return
	
	def get_file_name(self):
		return self.file_name
	
	def get_name(self):
		return self.info['Name']
		
	def get_description(self):
		return self.info['Description']
		
	def has_preferences(self):
		return self.info['Preferences']
		
	def call(self, name, *args):
		if hasattr(self.module, name):
			func = getattr(self.module, name)
			if callable(func):
				return apply(func, args)
