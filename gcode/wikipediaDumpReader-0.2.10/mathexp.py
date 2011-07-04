import commands
import tempfile
import os
#import md5
from os.path import join as J

class MathExp(object):
    """Math parser superclass. 
    Every parser object must implement a parse method that takes a latex string
    and returns an expression that can be displayed withing QTextBrowser.
    """
    def __init__(self, keepMathImageFiles):
        #math_parser = config.math_parser
        #self.math_parser = globals()[math_parser]()
	self.math_parser = Texvc(keepMathImageFiles)
    def parse_exp(self, m):
        math_reg_exp = m.group(1)
        try:
            output = self.math_parser.parse(math_reg_exp)
        except:
            output = '<p style="color: red;">(Failed to render) %s</p>' % math_reg_exp
        return output

class Texvc(object):

    def __init__(self, keepMathImageFiles = False):
	self.texvc = J(os.path.dirname(__file__), 'texvc')
	if not os.path.exists(self.texvc):
		self.texvc = "./texvc"
	# Test the program
	if commands.getstatusoutput(self.texvc)[0] != 0:
		self.texvc = "texvc"
		if commands.getstatusoutput(self.texvc)[0] != 0:
			raise NameError
	self.images_path = J(tempfile.gettempdir(), 'wikipediaDumpReader_texvm/')
	if os.path.exists(self.images_path):
		if not keepMathImageFiles:
			files_to_remove = os.listdir(self.images_path)
			for f in files_to_remove:
				os.remove(self.images_path + f)
	else:
		os.mkdir(self.images_path)
	# test again (paranoid mode ;-)
	test = self.parse("test")
	if not os.path.exists(test[26:-3]):
		raise NameError
	if keepMathImageFiles:
		print "Info : maths images are generated in %s and are not deleted after exiting" % self.images_path

    def parse(self, m):
	# tried to implement a cache system
	# texvc seems to change the string on which it computes the md5 (eg. a^{1} => a^{{1}})
	#prehash = md5.md5(m.replace('{', '{{').replace('}','}}')).hexdigest() # bug when frac{{a}}
	#prehash = md5.md5(m).hexdigest()
	#print "\n"
	#print m
	#print prehash
	#if not os.path.exists("%s%s.png" % (self.images_path, prehash)):
	if 1:
		print "Rendering mathematics, please wait slightly"
		cmd = self.texvc + " " + self.images_path + " "+ self.images_path + " '" + m + "' utf-8" + " 2>/dev/null"
		status, output = commands.getstatusoutput(cmd)
		prehash = output[1:33]
	return '<img valign="middle" src="%s%s.png"/>' % (self.images_path, prehash) # md5 is 32char, first char is texvm's output code
        #temp_hash = str.rindex(output, "\n") + 1
        #texvc_flag = output[temp_hash]
        #texvc_hash = output[temp_hash + 1 : temp_hash + 33]
        #parsed_math = '<img src="%s%s.png"/>' % (self.images_path, texvc_hash)
        #return parsed_math
