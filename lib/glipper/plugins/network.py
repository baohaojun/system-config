import threading, socket, os, stat, os.path, glipper, time
from gettext import gettext as _

cryptAvailable = True
try:
	from Crypto.Cipher import AES
except:
	cryptAvailable = False

GLIPPERPORT = 10368

allConnections = []
inserting = False
running = True

def info():
	info = {"Name": _("Network"),
		"Description": _("Connect multiple Glipper processes via network to synchronize their history"),
		"Preferences": True}
	return info

def on_new_item(item):
	if inserting:
		return
	if item == '':
		return
	StringSender(item).start()

#sends a new item to the other connections:
class StringSender(threading.Thread):
	def __init__(self, item):
		threading.Thread.__init__(self)
		self.item = item

	def run(self):
		for listener in allConnections:
			try:
				send = listener.encrypt(self.item)
				for c in range(len(send)/4096 +1):
					listener.socket.send(send[c*4096:c*4096+4096])
				listener.socket.send("\0")
			except socket.error:
				listener.quit()

class PasswordException(Exception):
	pass

#listens for new items from the other side of the connection:
class StringListener(threading.Thread):
	def __init__(self, socket, password, establisher):
		threading.Thread.__init__(self)
		socket.setblocking(True)
		global allConnections
		allConnections.append(self)
		self.establisher = establisher
		self.socket = socket
		self.password = password
		#get the AES object (password must be 16, 24 or 32 bytes long)
		if password != "":
			if not cryptAvailable:
				return	#TODO
			if len(password) <= 8:
				fill = 8
			fill += 8 - (len(password) % 8)
			passw = password + "\0" * fill
			self.crypt = AES.new(passw, AES.MODE_ECB)

	def encrypt(self, string):
		if self.password == "":
			return string
		#blocks must be 16*N bytes long
		return self.crypt.encrypt(string+"\0"*(16-len(string)%16))

	def decrypt(self, input):
		if self.password == "":
			return input
		string = self.crypt.decrypt(input)
		string = string[:string.find("\0")]
		return string

	def request_password_compare(self):
		en = self.encrypt("request")
		self.socket.send(en)
		response = self.socket.recv(4096)
		if response == "wrong!" or self.decrypt(response) != "response":
			raise PasswordException()

	def response_password_compare(self):
		request = self.socket.recv(4096)
		if self.decrypt(request) != "request":
			self.socket.send("wrong!")
			raise PasswordException()
		en = self.encrypt("response")
		self.socket.send(en)

	def quit(self):
		try:
			print "closing connection %s" % self.socket.getpeername()[0]
		except:
			print "closing connection"
		global allConnections
		allConnections.remove(self)
		self.socket.close()

	def run(self):
		print "comparing passwords:"
		try:
			if self.establisher:
				self.request_password_compare()
			else:
				self.response_password_compare()
		except socket.error:
			self.quit()
			return
		except PasswordException:
			print "wrong password for connection %s" % self.socket.getpeername()[0]
			self.quit()
			return

		print "password ok! start listening"

		def recv():
			string = self.socket.recv(4096)
			if not string:
				raise socket.error
			return string

		while True:	#loop quits when socket gets closed
			try:
				item = ""
				string = recv()
				while string != "\0":
					item += string
					string = recv()
				item = self.decrypt(item)
			except socket.error:
				self.quit()
				return
				
			global inserting
			inserting = True
			glipper.add_history_item(item)
			inserting = False
		
#listens for incoming connections (like a server does):
class ServerListener(threading.Thread):
	def __init__(self, acceptIPs):
		threading.Thread.__init__(self)
		self.acceptIPs = acceptIPs

	def run(self):
		print "start listening for incoming connections!"
		s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		s.bind(('', GLIPPERPORT))
		s.setblocking(False)
		
		while running:
			time.sleep(0.1)
			try:
				s.listen(1)
				conn, addr = s.accept()
				accept = False
				for x in self.acceptIPs:
					if socket.inet_aton(x[0]) == socket.inet_aton(addr[0]):
						accept = True
						password = x[1]
						break
				if accept:
					print "connection %s accepted" % addr[0]
				else:
					print "connection %s not accepted" % addr[0]
					raise socket.error()
			except socket.error:
				continue
				
			listener = StringListener(conn, password, False)
			listener.setDaemon(1)
			listener.start()
		print "stop listening for incoming connections!"
		s.close()
		
def stop():
	for c in allConnections:
		c.quit()
	global running
	running = False
	
def init():
	#read configfile:
	f = confFile("r")
	acceptIPs = f.getAcceptIPs()
	connectIPs = f.getConnectIPs()
	f.close()

	#First connect:
	for x in connectIPs:
		try:
			s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
			s.connect((x[0], GLIPPERPORT))
			print "connected to %s" % x[0]
		except socket.error:
			print "can\'t connect to %s" % x[0]
			s.close()
			continue
		listener = StringListener(s, x[1], True)
		listener.setDaemon(1)
		listener.start()

	#Then listen:
	server = ServerListener(acceptIPs)
	server.setDaemon(1)
	server.start()
	
def on_show_preferences(parent):
	preferences(parent).show()

#config file class:

class confFile:
	def __init__(self, mode):
		self.mode = mode

		conf_path = os.path.join(glipper.USER_PLUGINS_DIR, 'network.conf')
		if (mode == "r") and (not os.path.exists(conf_path)):
			self.accept = ()
			self.connect = ()
			return
		self.file = open(conf_path, mode)
		os.chmod(conf_path, stat.S_IRUSR | stat.S_IWUSR)

		if mode == "r":
			self.accept = []
			self.connect = []
			line = self.file.readline()
			while line != "\n":
				IP = line[:-1]
				line = self.file.readline()
				password = line[:-1]
				self.accept.append((IP, password))	
				line = self.file.readline()
			line = self.file.readline()
			while line != "":
				IP = line[:-1]
				line = self.file.readline()
				password = line[:-1]
				self.connect.append((IP, password))	
				line = self.file.readline()

	def setAcceptIPs(self, ips):
		self.accept = ips
	def getAcceptIPs(self):
		return self.accept
	def setConnectIPs(self, ips):
		self.connect = ips
	def getConnectIPs(self):
		return self.connect
	def close(self):
		if not 'file' in dir(self):
			return
		try:
			if self.mode == "w":
				for x in self.accept:
					self.file.write(x[0] + "\n")
					self.file.write(x[1] + "\n")
				self.file.write("\n")
				for x in self.connect:
					self.file.write(x[0] + "\n")
					self.file.write(x[1] + "\n")
		finally:
			self.file.close()

#preferences dialog:

import gtk

class preferences:
	def __init__(self, parent):
		builder_file = gtk.Builder()
		builder_file.add_from_file(os.path.join(os.path.dirname(__file__), "network.ui"))
		self.prefWind = builder_file.get_object("preferences")
		self.prefWind.set_transient_for(parent)
		self.prefWind.connect('response', self.on_prefWind_response)

		#Acception List:
		self.acceptList = builder_file.get_object("acceptList")
		self.acceptStore = gtk.ListStore(str, str)
		self.acceptList.set_model(self.acceptStore)
		renderer = gtk.CellRendererText()
		column = gtk.TreeViewColumn("IP", renderer, text=0)
		self.acceptList.append_column(column) 
		column = gtk.TreeViewColumn(_("Password"), renderer, text=1)
		self.acceptList.append_column(column) 

		#Connection List:
		self.connectList = builder_file.get_object("connectList")
		self.connectStore = gtk.ListStore(str, str)
		self.connectList.set_model(self.connectStore)
		renderer = gtk.CellRendererText()
		column = gtk.TreeViewColumn("IP", renderer, text=0)
		self.connectList.append_column(column) 
		column = gtk.TreeViewColumn(_("Password"), renderer, text=1)
		self.connectList.append_column(column) 

		builder_file.connect_signals({
			'on_addAccButton_clicked': self.on_addAccButton_clicked,
			'on_addConnButton_clicked': self.on_addConnButton_clicked,
			'on_delAccButton_clicked': self.on_delAccButton_clicked,
			'on_delConnButton_clicked': self.on_delConnButton_clicked,
		})

		#read configurations
		f = confFile("r")
		self.setStringListToStore(self.acceptStore, f.getAcceptIPs())
		self.setStringListToStore(self.connectStore, f.getConnectIPs())
		f.close()

	def show(self):
		self.prefWind.show_all()
		if not cryptAvailable:
			warning = gtk.MessageDialog(self.prefWind, gtk.DIALOG_MODAL, gtk.MESSAGE_WARNING, gtk.BUTTONS_OK, 
				_("You have to install the Python Cryptography Toolkit (python-crypto) if you want to use encrypted connections!"))
			warning.run()
			warning.destroy()

	def askIP(self):
		dialog = gtk.Dialog(_("New connection"), self.prefWind, 
				gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
				(gtk.STOCK_CANCEL, gtk.RESPONSE_REJECT,
				 gtk.STOCK_OK, gtk.RESPONSE_ACCEPT))
		dialog.set_default_size (350, 150)
		info = gtk.Label(_("Enter IP address: "))
		dialog.vbox.pack_start(info)
		info.show()
		entry = gtk.Entry()
		dialog.vbox.pack_start(entry)
		entry.show()
		passInfo = gtk.Label(_("Enter Password for this Connection: "))
		dialog.vbox.pack_start(passInfo)
		passInfo.show()
		passEntry = gtk.Entry()
		dialog.vbox.pack_start(passEntry)
		passEntry.show()
		res = dialog.run()
		dialog.destroy()
		if (res == gtk.RESPONSE_REJECT) or (entry.get_text() == ""):
			return None
		if res == gtk.RESPONSE_ACCEPT:
			if len(passEntry.get_text()) > 32:
				return None		# TODO: Show an error message
			return (entry.get_text(), passEntry.get_text())

	def addIPToList(self, store):
		result = self.askIP()
		if result != None:
			iter = store.append()
			store.set(iter, 0, result[0])
			store.set(iter, 1, result[1])

	def removeIPFromList(self, list):
		selection = list.get_selection()
		model, iter = selection.get_selected()
		if iter != None:
			model.remove(iter)

	def getStringListFromStore(self, store):
		iter = store.get_iter_first()
		while iter != None:
			yield (store.get_value(iter, 0), store.get_value(iter, 1))
			iter = store.iter_next(iter)

	def setStringListToStore(self, store, list):
		for x in list:
			iter = store.append()
			store.set(iter, 0, x[0])
			store.set(iter, 1, x[1])

	#EVENTS:

	def on_delAccButton_clicked(self, widget):
		self.removeIPFromList(self.acceptList)

	def on_addAccButton_clicked(self, widget):
		self.addIPToList(self.acceptStore)

	def on_delConnButton_clicked(self, widget):
		self.removeIPFromList(self.connectList)

	def on_addConnButton_clicked(self, widget):
		self.addIPToList(self.connectStore)

	def on_prefWind_response(self, widget, response):
		if response == gtk.RESPONSE_DELETE_EVENT or response == gtk.RESPONSE_CLOSE:
			f = confFile("w")
			f.setAcceptIPs(self.getStringListFromStore(self.acceptStore))
			f.setConnectIPs(self.getStringListFromStore(self.connectStore))
			f.close()
			widget.destroy()
